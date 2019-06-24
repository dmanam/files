-- | A vertical bar that can plot data in the range [0, 1] and display an X.  The
-- colors are configurable.
module System.Taffybar.Widget.Generic.VerticalBarX (
  -- * Types
  VerticalBarXHandle,
  BarConfig(..),
  BarXXConfig(..),
  BarDirection(..),
  -- * Accessors/Constructors
  verticalBarXNew,
  verticalBarXSetState,
  defaultBarConfig,
  defaultBarConfigIO,
  defaultBarXXConfig
  ) where

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class
import qualified GI.Cairo.Render as C
import           GI.Cairo.Render.Connector
import           GI.Gtk hiding (widgetGetAllocatedSize)
import           System.Taffybar.Util
import           System.Taffybar.Widget.Util

import System.Taffybar.Widget.Generic.VerticalBar (
  BarDirection(..),
  BarConfig(..),
  defaultBarConfig,
  defaultBarConfigIO,
  )

newtype VerticalBarXHandle = VBH (MVar VerticalBarXState)
data VerticalBarXState = VerticalBarXState
  { barXIsBootstrapped :: Bool
  , barXPercent :: Double
  , barXX :: Bool
  , barXCanvas :: DrawingArea
  , barXConfig :: BarConfig
  , barXXConfig :: BarXXConfig
  }

data BarXXConfig
  = BarXXConfig {
       barXXColor :: (Double, Double, Double)
     , barXXPadding :: Int}

defaultBarXXConfig :: BarXXConfig
defaultBarXXConfig = BarXXConfig (1, 0, 0) 0

verticalBarXSetState :: VerticalBarXHandle -> (Double, Bool) -> IO ()
verticalBarXSetState (VBH mv) (pct, x) = do
  s <- readMVar mv
  let drawArea = barXCanvas s
  when (barXIsBootstrapped s) $ do
    modifyMVar_ mv (\s' -> return s' { barXPercent = clamp 0 1 pct })
    modifyMVar_ mv (\s' -> return s' { barXX = x })
    postGUIASync $ widgetQueueDraw drawArea

-- begin unmodified code
clamp :: Double -> Double -> Double -> Double
clamp lo hi d = max lo $ min hi d

liftedBackgroundColor :: BarConfig -> Double -> IO (Double, Double, Double)
liftedBackgroundColor bc pct =
  case bc of
    BarConfig { barBackgroundColor = bcolor } -> return (bcolor pct)
    BarConfigIO { barBackgroundColorIO = bcolor } -> bcolor pct

liftedBorderColor :: BarConfig -> IO (Double, Double, Double)
liftedBorderColor bc =
  case bc of
    BarConfig { barBorderColor = border } -> return border
    BarConfigIO { barBorderColorIO = border } -> border

liftedBarColor :: BarConfig -> Double -> IO (Double, Double, Double)
liftedBarColor bc pct =
  case bc of
    BarConfig { barColor = c } -> return (c pct)
    BarConfigIO { barColorIO = c } -> c pct

renderFrame_ :: Double -> BarConfig -> Int -> Int -> C.Render ()
renderFrame_ pct cfg width height = do
  let fwidth = fromIntegral width
      fheight = fromIntegral height

  -- Now draw the user's requested background, respecting padding
  (bgR, bgG, bgB) <- C.liftIO $ liftedBackgroundColor cfg pct
  let pad = barPadding cfg
      fpad = fromIntegral pad
  C.setSourceRGB bgR bgG bgB
  C.rectangle fpad fpad (fwidth - 2 * fpad) (fheight - 2 * fpad)
  C.fill

  -- Now draw a nice frame
  (frameR, frameG, frameB) <- C.liftIO $ liftedBorderColor cfg
  C.setSourceRGB frameR frameG frameB
  C.setLineWidth 1.0
  C.rectangle (fpad + 0.5) (fpad + 0.5) (fwidth - 2 * fpad - 1) (fheight - 2 * fpad - 1)
  C.stroke
-- end unmodified code

renderBar :: Double -> Bool -> BarConfig -> BarXXConfig -> Int -> Int -> C.Render ()
renderBar pct x cfg xfg width height = do
  let direction = barDirection cfg
      activeHeight = case direction of
                       VERTICAL   -> pct * fromIntegral height
                       HORIZONTAL -> fromIntegral height
      activeWidth  = case direction of
                       VERTICAL   -> fromIntegral width
                       HORIZONTAL -> pct * fromIntegral width
      newOrigin    = case direction of
                       VERTICAL -> fromIntegral height - activeHeight
                       HORIZONTAL -> 0
      pad = barPadding cfg

  renderFrame_ pct cfg width height

  ctm <- C.getMatrix

  -- After we draw the frame, transform the coordinate space so that
  -- we only draw within the frame.
  C.translate (fromIntegral pad + 1) (fromIntegral pad + 1)
  let xS = fromIntegral (width - 2 * pad - 2) / fromIntegral width
      yS = fromIntegral (height - 2 * pad - 2) / fromIntegral height
  C.scale xS yS

  (r, g, b) <- C.liftIO $ liftedBarColor cfg pct
  C.setSourceRGB r g b
  C.translate 0 newOrigin
  C.rectangle 0 0 activeWidth activeHeight
  C.fill

  when x $ do
    C.setMatrix ctm
    let pad = barXXPadding xfg
        (r, g, b) = barXXColor xfg
        d = min (width - 2 * pad) (height - 2 * pad)
        [x0, y0, x1, y1] = (\f o -> fromIntegral (f o d) / 2) <$> [(-), (+)] <*> [width, height]
    C.moveTo x0 y0
    C.lineTo x1 y1
    C.moveTo x0 y1
    C.lineTo x1 y0
    C.setSourceRGB r g b
    C.setLineWidth 2.0
    C.stroke

drawBar :: MVar VerticalBarXState -> DrawingArea -> C.Render ()
drawBar mv drawArea = do
  (w, h) <- widgetGetAllocatedSize drawArea
  s <- liftIO $ do
         s <- readMVar mv
         modifyMVar_ mv (\s' -> return s' { barXIsBootstrapped = True })
         return s
  renderBar (barXPercent s) (barXX s) (barXConfig s) (barXXConfig s) w h

verticalBarXNew :: MonadIO m => BarConfig -> BarXXConfig -> m (GI.Gtk.Widget, VerticalBarXHandle)
verticalBarXNew cfg xfg = liftIO $ do
  drawArea <- drawingAreaNew
  mv <-
    newMVar
      VerticalBarXState
      { barXIsBootstrapped = False
      , barXPercent = 0
      , barXX = False
      , barXCanvas = drawArea
      , barXConfig = cfg
      , barXXConfig = xfg
      }
  widgetSetSizeRequest drawArea (fromIntegral $ barWidth cfg) (-1)
  _ <- onWidgetDraw drawArea $ \ctx -> renderWithContext (drawBar mv drawArea) ctx >> return True
  box <- boxNew OrientationHorizontal 1
  boxPackStart box drawArea True True 0
  widgetShowAll box
  giBox <- toWidget box
  return (giBox, VBH mv)
