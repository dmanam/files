{-# LANGUAGE ScopedTypeVariables #-}
-- | This is a graph widget inspired by the widget of the same name in
-- Awesome (the window manager).  It plots a series of data points
-- similarly to a bar graph.  This version must be explicitly fed data
-- with 'graphAddSample'.  For a more automated version, see
-- 'PollingGraph'.
--
-- Like Awesome, this graph can plot multiple data sets in one widget.
-- The data sets are plotted in the order provided by the caller.
--
-- Note: all of the data fed to this widget should be in the range
-- [0,1].
module System.Taffybar.Widget.Generic.OverlayGraph (
  -- * Types
    OverlayGraphHandle
  , GraphConfig(..)
  , GraphOverlayConfig(..)
  , GraphDirection(..)
  , GraphStyle(..)
  -- * Functions
  , oGraphNew
  , oGraphAddSample
  , oGraphUpdateOverlay
  , oGraphUpdateColors
  , defaultGraphConfig
  , defaultGraphOverlayConfig
  ) where

import           Control.Concurrent
import           Control.Monad ( when )
import           Control.Monad.IO.Class
import           Data.Foldable ( mapM_ )
import           Data.Sequence ( Seq, (<|), viewl, ViewL(..) )
import qualified Data.Sequence as S
import qualified Data.Text as T
import           Data.Monoid ((<>))
import qualified GI.Cairo.Render as C
import           GI.Cairo.Render.Connector as C
import qualified GI.Cairo.Render.Matrix as M
import qualified GI.Gtk as Gtk
import qualified GI.PangoCairo as P
import qualified GI.Pango as P
import           Prelude hiding ( mapM_ )
import           System.Taffybar.Util
import           System.Taffybar.Widget.Util

import System.Taffybar.Widget.Generic.Graph (GraphConfig(..), GraphDirection(..), GraphStyle(..), defaultGraphConfig)

newtype OverlayGraphHandle = OGH (MVar OverlayGraphState)
data OverlayGraphState =
  OverlayGraphState { oGraphIsBootstrapped :: Bool
                    , oGraphHistory :: [Seq Double]
                    , oGraphOverlay :: T.Text
                    , oGraphCanvas :: Gtk.DrawingArea
                    , oGraphLabel :: Gtk.Label
                    , oGraphConfig :: GraphConfig
                    , oGraphOverlayConfig :: GraphOverlayConfig
                    }

type RGBA = (Double, Double, Double, Double)

data GraphOverlayConfig = GraphOverlayConfig {
    oGraphOverlayFGColor :: RGBA
  , oGraphOverlayBGColor :: RGBA
  }

defaultGraphOverlayConfig :: GraphOverlayConfig
defaultGraphOverlayConfig = GraphOverlayConfig (0.0, 0.0, 0.0, 1.0) (0.5, 0.5, 0.5, 1.0)

-- | Add a data point to the graph for each of the tracked data sets.
-- There should be as many values in the list as there are data sets.
oGraphAddSample :: OverlayGraphHandle -> [Double] -> IO ()
oGraphAddSample (OGH mv) rawData = do
  s <- readMVar mv
  let drawArea = oGraphCanvas s
      histSize = graphHistorySize (oGraphConfig s)
      histsAndNewVals = zip pcts (oGraphHistory s)
      newHists = case oGraphHistory s of
        [] -> map S.singleton pcts
        _ -> map (\(p,h) -> S.take histSize $ p <| h) histsAndNewVals
  when (oGraphIsBootstrapped s) $ do
    modifyMVar_ mv (\s' -> return s' { oGraphHistory = newHists })
    postGUIASync $ Gtk.widgetQueueDraw drawArea
  where
    pcts = map (clamp 0 1) rawData
oGraphUpdateOverlay :: OverlayGraphHandle -> T.Text -> IO ()
oGraphUpdateOverlay (OGH mv) newText = do
  s <- readMVar mv
  let drawArea = oGraphCanvas s
  when (oGraphIsBootstrapped s) $ do
    modifyMVar_ mv (\s' -> return s' { oGraphOverlay = newText })
    postGUIASync $ Gtk.widgetQueueDraw drawArea
oGraphUpdateColors :: OverlayGraphHandle -> [RGBA] -> IO ()
oGraphUpdateColors (OGH mv) newColors = do
  s <- readMVar mv
  let drawArea = oGraphCanvas s
      cfg = oGraphConfig s
  when (oGraphIsBootstrapped s) $ do
    modifyMVar_ mv (\s' -> return s' { oGraphConfig = cfg { graphDataColors = newColors} })
    postGUIASync $ Gtk.widgetQueueDraw drawArea

clamp :: Double -> Double -> Double -> Double
clamp lo hi d = max lo $ min hi d

outlineData :: (Double -> Double) -> Double -> Double -> C.Render ()
outlineData pctToY xStep pct = do
  (curX,_) <- C.getCurrentPoint
  C.lineTo (curX + xStep) (pctToY pct)

renderFrameAndBackground :: GraphConfig -> Int -> Int -> C.Render ()
renderFrameAndBackground cfg w h = do
  let (backR, backG, backB, backA) = graphBackgroundColor cfg
      (frameR, frameG, frameB, frameA) = graphBorderColor cfg
      pad = graphPadding cfg
      fpad = fromIntegral pad
      fw = fromIntegral w
      fh = fromIntegral h

  -- Draw the requested background
  C.setSourceRGBA backR backG backB backA
  C.rectangle fpad fpad (fw - 2 * fpad) (fh - 2 * fpad)
  C.fill

  -- Draw a frame around the widget area
  -- (unless equal to background color, which likely means the user does not
  -- want a frame)
  when (graphBorderWidth cfg > 0) $ do
    let p = fromIntegral (graphBorderWidth cfg)
    C.setLineWidth p
    C.setSourceRGBA frameR frameG frameB frameA
    C.rectangle (fpad + (p / 2)) (fpad + (p / 2)) (fw - 2 * fpad - p) (fh - 2 * fpad - p)
    C.stroke


renderGraph :: [Seq Double] -> P.Layout -> GraphConfig -> GraphOverlayConfig -> Int -> Int -> Double -> C.Render ()
renderGraph hists layout cfg ofg w h xStep = do
  renderFrameAndBackground cfg w h

  C.setLineWidth 0.1

  ctx <- getContext
  rl <- snd <$> P.layoutGetPixelExtents layout
  wl <- P.getRectangleWidth rl
  hl <- P.getRectangleHeight rl
  let [dx, dy] = zipWith (\a b -> (fromIntegral a - fromIntegral b) / 2) [w, h] [wl, hl]
      xl = dx
      yl = dy
      (r, g, b, a) = oGraphOverlayBGColor ofg
  C.setSourceRGBA r g b a
  C.moveTo xl yl
  ctm0 <- C.getMatrix
  P.showLayout ctx layout

  let pad = fromIntegral $ graphPadding cfg
  let framePad = fromIntegral $ graphBorderWidth cfg

  -- Make the new origin be inside the frame and then scale the
  -- drawing area so that all operations in terms of width and height
  -- are inside the drawn frame.
  C.translate (pad + framePad) (pad + framePad)
  let xS = (fromIntegral w - 2 * pad - 2 * framePad) / fromIntegral w
      yS = (fromIntegral h - 2 * pad - 2 * framePad) / fromIntegral h
  C.scale xS yS

  -- If right-to-left direction is requested, apply an horizontal inversion
  -- transformation with an offset to the right equal to the width of the widget.
  when (graphDirection cfg == RIGHT_TO_LEFT) $
      C.transform $ M.Matrix (-1) 0 0 1 (fromIntegral w) 0

  let pctToY pct = fromIntegral h * (1 - pct)
      renderDataSet hist color style
        | S.length hist <= 1 = return ()
        | otherwise = do
          let (r, g, b, a) = color
              originY = pctToY newestSample
              originX = 0
              newestSample :< hist' = viewl hist
          C.setSourceRGBA r g b a
          C.moveTo originX originY

          mapM_ (outlineData pctToY xStep) hist'
          case style of
            Area -> do
              (endX, _) <- C.getCurrentPoint
              C.lineTo endX (fromIntegral h)
              C.lineTo 0 (fromIntegral h)
              C.clipPreserve
              C.fill
              let (r, g, b, a) = oGraphOverlayFGColor ofg
              C.setSourceRGBA r g b a
              ctm1 <- C.getMatrix
              C.setMatrix ctm0
              C.moveTo xl yl
              P.showLayout ctx layout
              C.setMatrix ctm1
            Line -> do
              C.setLineWidth 1.0
              C.stroke


  sequence_ $ zipWith3 renderDataSet hists (graphDataColors cfg) (graphDataStyles cfg)

drawBorder :: MVar OverlayGraphState -> Gtk.DrawingArea -> C.Render ()
drawBorder mv drawArea = do
  (w, h) <- widgetGetAllocatedSize drawArea
  s <- liftIO $ readMVar mv
  let cfg = oGraphConfig s
  renderFrameAndBackground cfg w h
  liftIO $ modifyMVar_ mv (\s' -> return s' { oGraphIsBootstrapped = True })
  return ()

drawGraph :: MVar OverlayGraphState -> Gtk.DrawingArea ->  C.Render ()
drawGraph mv drawArea = do
  (w, h) <- widgetGetAllocatedSize drawArea
  drawBorder mv drawArea
  s <- liftIO $ readMVar mv
  let hist = oGraphHistory s
      overlay = oGraphOverlay s
      cfg = oGraphConfig s
      ofg = oGraphOverlayConfig s
      label = oGraphLabel s
      histSize = graphHistorySize cfg
      -- Subtract 1 here since the first data point doesn't require
      -- any movement in the X direction
      xStep = fromIntegral w / fromIntegral (histSize - 1)

  -- this is pretty hacky
  Gtk.labelSetMarkup label overlay
  layout <- Gtk.labelGetLayout label
  Gtk.labelSetMarkup label T.empty

  case hist of
    [] -> renderFrameAndBackground cfg w h
    _ -> renderGraph hist layout cfg ofg w h xStep

oGraphNew :: MonadIO m => GraphConfig -> GraphOverlayConfig -> m (Gtk.Widget, OverlayGraphHandle)
oGraphNew cfg ofg = liftIO $ do
  drawArea <- Gtk.drawingAreaNew
  label <- Gtk.labelNew (Nothing :: Maybe T.Text)
  mv <- newMVar OverlayGraphState { oGraphIsBootstrapped = False
                                  , oGraphHistory = []
                                  , oGraphOverlay = T.empty
                                  , oGraphCanvas = drawArea
                                  , oGraphLabel = label
                                  , oGraphConfig = cfg
                                  , oGraphOverlayConfig = ofg
                                  }

  ov <- Gtk.overlayNew
  Gtk.containerAdd ov drawArea
  Gtk.overlayAddOverlay ov label

  Gtk.widgetSetSizeRequest drawArea (fromIntegral $ graphWidth cfg) (-1)

  _ <- Gtk.onWidgetDraw drawArea (\ctx -> C.renderWithContext (drawGraph mv drawArea) ctx >> return True)

  Gtk.widgetSetVexpand drawArea True
  Gtk.widgetSetVexpand ov True
  Gtk.widgetShowAll ov
  giWidget <- Gtk.toWidget ov
  return (giWidget, OGH mv)
