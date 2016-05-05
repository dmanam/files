import System.Taffybar

import System.Taffybar.Systray
import System.Taffybar.Pager
import System.Taffybar.TaffyPager
import System.Taffybar.SimpleClock
import System.Taffybar.FreedesktopNotifications
import System.Taffybar.Weather
import System.Taffybar.MPRIS
import System.Taffybar.MPRIS2
import System.Taffybar.Battery

import System.Taffybar.Widgets.PollingBar
import System.Taffybar.Widgets.PollingGraph

import System.Information.Memory
import System.Information.CPU

import System.Environment
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Graphics.X11.Xlib.Display
import Graphics.X11.XRM

memCallback = do
  mi <- parseMeminfo
  return [memoryUsedRatio mi]

cpuCallback = do
  (userLoad, systemLoad, totalLoad) <- cpuLoad
  return [totalLoad]

memCfg = defaultGraphConfig { graphDataColors = [(1, 95/255, 95/255, 1)]
                            , graphLabel = Just "mem"
                            , graphDirection = RIGHT_TO_LEFT
                            }
cpuCfg = defaultGraphConfig { graphDataColors = [(175/255, 215/255, 135/255, 1)]
                            , graphLabel = Just "cpu"
                            , graphDirection = RIGHT_TO_LEFT
                            }
clock colors = textClockNew Nothing ("<span fgcolor='" ++ colors !! 3 ++ "'>%a %b %_d %H:%M</span>") 1
pager colors = taffyPagerNew $ defaultPagerConfig { activeWindow     = escape . shorten 160
                                                  , activeLayout     = colorize (colors !! 16) (colors !! 18) . wrap " " " " . escape
                                                  , activeWorkspace  = colorize (colors !! 16) (colors !! 18) . wrap " " " "  . escape
                                                  , hiddenWorkspace  = escape
                                                  , emptyWorkspace   = const ""
                                                  , visibleWorkspace = wrap "(" ")" . escape
                                                  , urgentWorkspace  = colorize (colors !! 17) (colors !!  1) . wrap "‼" "‼" . escape
                                                  , widgetSep        = "  "
                                                  }
note = notifyAreaNew defaultNotificationConfig
wea colors = weatherNew ((defaultWeatherConfig "KBOS") {weatherTemplate = "$tempF$°F", weatherFormatter = WeatherFormatter ((++ "°F") . fmt)}) 10
  where fmt WI {tempF = temp}
          | temp > 85 = colorize (colors !! 1) "" . show $ temp
          | temp > 78 = colorize (colors !! 3) "" . show $ temp
          | temp > 60 = colorize (colors !! 2) "" . show $ temp
          | otherwise = colorize (colors !! 4) "" . show $ temp
--mpris = mprisNew defaultMPRISConfig
--mpris2 = mpris2New
mem = pollingGraphNew memCfg 1 memCallback
cpu = pollingGraphNew cpuCfg 0.5 cpuCallback
tray = systrayNew
battxt = textBatteryNew "($time$)" 30
batbar = batteryBarNew (defaultBarConfig colorFunc) 60
  where colorFunc pct
          | pct > 0.5 = ((255 - 80 * pct)/255, 215/255, (215 - 80 * pct)/255)
          | pct < 0.5 = ((255 - 80 * pct)/255, (95 + 240 * pct)/255, (95 + 160 * pct)/255)
          | otherwise = (215/255, 215/255, 175/255)
--        | pct > 0.5 = ((1 - pct)/pct, 1,             0)
--        | pct < 0.5 = (1,             pct/(1 - pct), 0)
--        | otherwise = (1,             1,             0)

main = do
  display <- openDisplay =<< getEnv "DISPLAY"
  let getRes = MaybeT . getDefault display "taffybar"
  runMaybeT $ do
    colors <- sequence . fmap getRes $ fmap (("color" ++) . show) [0..15] ++ ["background", "foreground", "cursorColor"]
    lift . defaultTaffybar $ defaultTaffybarConfig { startWidgets = [pager colors]
                                                   , endWidgets = [tray, wea colors, clock colors, mem, cpu, battxt, batbar, note]
                                                   }
