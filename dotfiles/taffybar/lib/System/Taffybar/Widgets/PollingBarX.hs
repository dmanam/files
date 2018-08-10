-- | A vertical bar that can plot data in the range [0, 1].  The
-- colors are configurable.
module System.Taffybar.Widgets.PollingBarX (
  -- * Types
  BarXConfig(..),
  BarDirection(..),
  -- * Accessors/Constructors
  pollingBarXNew,
  defaultBarXConfig
  ) where

import Control.Concurrent
import Control.Monad
import qualified Graphics.Rendering.Cairo as C
import Graphics.UI.Gtk
import qualified Control.Exception.Enclosed as E

import System.Taffybar.Widgets.PollingBar (BarDirection(..))

pollingBarXNew :: BarXConfig -> Double -> IO (Double, Bool) -> IO Widget
pollingBarXNew cfg pollSeconds action = do
  (drawArea, h) <- verticalBarNew cfg

  _ <- on drawArea realize $ do
    _ <- forkIO $ forever $ do
      esample <- E.tryAny action
      case esample of
        Left _ -> return ()
        Right sample -> verticalBarSetPercent h sample
      threadDelay $ floor (pollSeconds * 1000000)
    return ()

  return drawArea

newtype VerticalBarHandle = VBH (MVar VerticalBarState)
data VerticalBarState =
  VerticalBarState { barIsBootstrapped :: Bool
                   , barPercent :: Double
                   , xState :: Bool
                   , barCanvas :: DrawingArea
                   , barConfig :: BarXConfig
                   }

data BarXConfig =
  BarXConfig { barBorderColor :: (Double, Double, Double) -- ^ Color of the border drawn around the widget
             , barBackgroundColor :: Double -> (Double, Double, Double) -- ^ The background color of the widget
             , barColor :: Double -> (Double, Double, Double) -- ^ A function to determine the color of the widget for the current data point
             , xColor :: (Double, Double, Double)
             , barPadding :: Int -- ^ Number of pixels of padding around the widget
             , barWidth :: Int
             , xSize :: Int
             , barDirection :: BarDirection
             }

-- | A default bar configuration.  The color of the active portion of
-- the bar must be specified.
defaultBarXConfig :: BarXConfig
defaultBarXConfig = BarXConfig { barBorderColor = (0.5, 0.5, 0.5)
                               , barBackgroundColor = const (0, 0, 0)
                               , barColor = const (0.5, 0.5, 0.5)
                               , xColor = (1, 0, 0)
                               , barPadding = 2
                               , barWidth = 15
                               , xSize = 20
                               , barDirection = VERTICAL
                               }

verticalBarSetPercent :: VerticalBarHandle -> (Double, Bool) -> IO ()
verticalBarSetPercent (VBH mv) (pct, x) = do
  s <- readMVar mv
  let drawArea = barCanvas s
  case barIsBootstrapped s of
    False -> return ()
    True -> do
      modifyMVar_ mv (\s' -> return s' { barPercent = clamp 0 1 pct, xState = x })
      postGUIAsync $ widgetQueueDraw drawArea

clamp :: Double -> Double -> Double -> Double
clamp lo hi d = max lo $ min hi d

renderFrame :: Double -> BarXConfig -> Int -> Int -> C.Render ()
renderFrame pct cfg width height = do
  let fwidth = fromIntegral width
      fheight = fromIntegral height

  -- Now draw the user's requested background, respecting padding
  let (bgR, bgG, bgB) = barBackgroundColor cfg pct
      pad = barPadding cfg
      fpad = fromIntegral pad
  C.setSourceRGB bgR bgG bgB
  C.rectangle fpad fpad (fwidth - 2 * fpad) (fheight - 2 * fpad)
  C.fill

  -- Now draw a nice frame
  let (frameR, frameG, frameB) = barBorderColor cfg
  C.setSourceRGB frameR frameG frameB
  C.setLineWidth 1.0
  C.rectangle (fpad + 0.5) (fpad + 0.5) (fwidth - 2 * fpad - 1) (fheight - 2 * fpad - 1)
  C.stroke

renderBar :: Double -> Bool -> BarXConfig -> Int -> Int -> C.Render ()
renderBar pct x cfg fullWidth height = do
  let direction = barDirection cfg
      width = barWidth cfg
      xs = fromIntegral $ xSize cfg
      xpadding = (xs - fromIntegral width) / 2
      activeHeight = case direction of
                       VERTICAL   -> pct * (fromIntegral height)
                       HORIZONTAL -> fromIntegral height
      activeWidth  = case direction of
                       VERTICAL   -> fromIntegral width
                       HORIZONTAL -> pct * (fromIntegral width)
      newOrigin    = case direction of
                       VERTICAL -> fromIntegral height - activeHeight
                       HORIZONTAL -> 0
      pad = barPadding cfg

  when (xpadding > 0) $
    C.translate xpadding 0
  renderFrame pct cfg width height

  -- After we draw the frame, transform the coordinate space so that
  -- we only draw within the frame.
  C.translate (fromIntegral pad + 1) (fromIntegral pad + 1)
  let xS = fromIntegral (width - 2 * pad - 2) / fromIntegral width
      yS = fromIntegral (height - 2 * pad - 2) / fromIntegral height
  C.scale xS yS

  let (r, g, b) = (barColor cfg) pct
  C.setSourceRGB r g b
  C.translate 0 newOrigin
  C.rectangle 0 0 activeWidth activeHeight
  C.fill

  when x $ do
    C.identityMatrix
    let (r, g, b) = xColor cfg
        [x, y] = zipWith (\a b -> (fromIntegral a - b) / 2) [fullWidth, height] [xs, xs]
    C.moveTo x y
    C.lineTo (x + xs) (y + xs)
    C.moveTo x (y + xs)
    C.lineTo (x + xs) y
    C.setSourceRGB r g b
    C.setLineWidth 2.0
    C.stroke

drawBar :: MVar VerticalBarState -> DrawingArea -> IO ()
drawBar mv drawArea = do
  (w, h) <- widgetGetSize drawArea
  drawWin <- widgetGetDrawWindow drawArea
  s <- readMVar mv
  let pct = barPercent s
      x = xState s
  modifyMVar_ mv (\s' -> return s' { barIsBootstrapped = True })
  renderWithDrawable drawWin (renderBar pct x (barConfig s) w h)

verticalBarNew :: BarXConfig -> IO (Widget, VerticalBarHandle)
verticalBarNew cfg = do
  drawArea <- drawingAreaNew

  mv <- newMVar VerticalBarState { barIsBootstrapped = False
                                 , barPercent = 0
                                 , xState = False
                                 , barCanvas = drawArea
                                 , barConfig = cfg
                                 }

  let w = max (barWidth cfg) (xSize cfg)
  widgetSetSizeRequest drawArea w (-1)
  _ <- on drawArea exposeEvent $ tryEvent $ C.liftIO (drawBar mv drawArea)

  box <- hBoxNew False 1
  boxPackStart box drawArea PackGrow 0
  widgetShowAll box

  return (toWidget box, VBH mv)