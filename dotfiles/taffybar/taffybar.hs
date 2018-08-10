import System.Taffybar

import System.Taffybar.DiskIOMonitor
import System.Taffybar.CPUMonitor
import System.Taffybar.Systray
import System.Taffybar.Pager
import System.Taffybar.TaffyPager
import System.Taffybar.SimpleClock
import System.Taffybar.FreedesktopNotifications
import System.Taffybar.Weather
import System.Taffybar.Battery

import System.Taffybar.Widgets.PollingGraph
import System.Taffybar.Widgets.PollingOverlayGraph
import System.Taffybar.Widgets.PollingBarX

import System.Information.Memory
import System.Information.Network
import System.Information.Battery

import System.Process (readProcessWithExitCode)
import System.IO.Unsafe (unsafePerformIO)

import Data.Char (toUpper)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Text.Printf (printf)
import Control.Monad.Trans.Maybe
import Data.IORef (newIORef, readIORef, writeIORef)

colors = fmap ("#" ++)
  [ "121212"
  , "ff5f5f"
  , "afd787"
  , "d7d7af"
  , "87d7ff"
  , "d7afd7"
  , "87d7af"
  , "d7d7d7"
  , "121212"
  , "ff5f5f"
  , "afd787"
  , "d7d7af"
  , "87d7ff"
  , "d7afd7"
  , "87d7af"
  , "d7d7d7"
  , "000000"
  , "d7d7d7"
  , "87afd7"
  ]

memReader = do
  mi <- parseMeminfo
  return [memoryUsedRatio mi]

netMax = 10 * 1024 * 1024
netReader = let old = unsafePerformIO $ newIORef (-1, -1) in do
  (rx', tx') <- readIORef old
  rx:tx:_ <- fromMaybe [-1, -1] <$> getNetInfo "wlp2s0"
  writeIORef old (rx, tx)
  return $ (/ netMax) . fromInteger <$> [rx - rx', tx - tx']

batReader = fmap (fromMaybe (0, "", (0, 0, 0, 0))) . runMaybeT $ do
  ctx <- MaybeT batteryContextNew
  info <- MaybeT $ getBatteryInfo ctx
  let val = batteryPercentage info / 100
      formatTime seconds =
        let minutes = seconds `div` 60
            hours = minutes `div` 60
            minutes' = minutes `mod` 60
        in printf "%02d:%02d" hours minutes'
      battTime = case (batteryState info) of
        BatteryStateFullyCharged -> "full"
        BatteryStateCharging -> (formatTime $ batteryTimeToFull info)
        BatteryStateDischarging -> (formatTime $ batteryTimeToEmpty info)
        _ -> ""
      color = case (batteryState info) of
        BatteryStateFullyCharged -> (135/255, 215/255, 95/255, 1)
        BatteryStateCharging -> (175/255, 215/255, 135/255, 1)
        _ -> ((255 - 40 * val)/255, (95 + 120 * val)/255, (95 + 80 * val)/255, 1)
  return (val, battTime, color)

volReader = fmap (fromMaybe (-1, False)) . runMaybeT $ do
  let exec arg = MaybeT $ do
        (_, r:et, _) <- readProcessWithExitCode "pamixer" [arg] ""
        return $ readMaybe $ toUpper r:et
  vol  <- exec "--get-volume"
  mute <- exec "--get-mute"
  return (vol/100, mute)

graphcfg colors = defaultGraphConfig
  { graphLabel = Nothing
  , graphDirection = RIGHT_TO_LEFT
  , graphDataColors = colors
  }
memCfg = graphcfg [(1, 95/255, 95/255, 1)]
cpuCfg = graphcfg [(175/255, 215/255, 135/255, 1)]
netCfg = graphcfg [(135/255, 215/255, 1, 1), (235/255, 155/255, 135/255, 0.5)]
dioCfg = graphcfg [(215/255, 215/255, 175/255, 1)]
batCfg = defaultOverlayGraphConfig
  { ographHistorySize = 60
  , ographWidth = 80
  , ographDirection = RIGHT_TO_LEFT
  }
volCfg = defaultBarXConfig { xColor = (1, 95/255, 95/255) }

clock = textClockNew Nothing ("<span fgcolor='" ++ colors !! 3 ++ "'>%a %b %_d %H:%M:%S</span>") 1
pager = taffyPagerNew $ defaultPagerConfig
  { activeWindow     = wrap ("<span fgcolor='" ++ colors !! 2 ++ "'>") "</span>" . escape . shorten 160
  , activeLayout     = colorize (colors !! 16) (colors !! 18) . wrap " " " " . escape
  , activeWorkspace  = colorize (colors !! 16) (colors !! 18) . wrap " " " " . escape
  , hiddenWorkspace  = escape
  , emptyWorkspace   = const ""
  , visibleWorkspace = wrap "(" ")" . escape
  , urgentWorkspace  = colorize (colors !! 17) (colors !!  1) . wrap "‼" "‼" . escape
  , widgetSep        = "  "
  }
note = notifyAreaNew defaultNotificationConfig

--wea = weatherNew ((defaultWeatherConfig "KBOS") {weatherTemplate = "$tempC$°F", weatherFormatter = WeatherFormatter ((++ " K") . fmt)}) 10
--  where fmt WI {tempC = temp}
--          | temp > 30 = colorize (colors !! 1) "" . show $ temp + 273
--          | temp > 26 = colorize (colors !! 3) "" . show $ temp + 273
--          | temp > 15 = colorize (colors !! 2) "" . show $ temp + 273
--          | otherwise = colorize (colors !! 4) "" . show $ temp + 273
mem = pollingGraphNew memCfg 1 memReader
cpu = cpuMonitorNew cpuCfg 1 "cpu"
net = pollingGraphNew netCfg 1 netReader
dio = dioMonitorNew dioCfg 1 "sda"
bat = pollingOverlayGraphNew batCfg 1 60 batReader
vol = pollingBarXNew volCfg 0.25 volReader

tray = systrayNew
--battxt = textBatteryNew "$time$" 30
--batbar = batteryBarNew (defaultBarConfig colorFunc) 60
--  where colorFunc pct
--          | pct > 0.5 = ((255 - 80 * pct)/255, 215/255, (215 - 80 * pct)/255)
--          | pct < 0.5 = ((255 - 80 * pct)/255, (95 + 240 * pct)/255, (95 + 160 * pct)/255)
--          | otherwise = (215/255, 215/255, 175/255)

main = do
  -- display <- openDisplay =<< getEnv "DISPLAY"
  -- let getRes = MaybeT . getDefault display "taffybar"
  -- colors <- sequence . fmap getRes $ fmap (("color" ++) . show) [0..15] ++ ["background", "foreground", "cursorColor"]
  defaultTaffybar $ defaultTaffybarConfig { startWidgets = [pager]
                                          , endWidgets = [clock, bat, dio, net, mem, cpu, vol, tray, note]
                                          , barHeight = 30
                                          }
