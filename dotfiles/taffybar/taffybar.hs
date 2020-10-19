{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

import System.Taffybar

import System.Taffybar.SimpleConfig as SC

import System.Taffybar.Widget

import System.Taffybar.Widget.BetterLayout
import System.Taffybar.Widget.BetterWeather as Wea
import System.Taffybar.Widget.Volume as Vol

import System.Taffybar.Widget.Generic.PollingGraph
import System.Taffybar.Widget.Generic.PollingOverlayGraph

import System.Taffybar.Information.Memory
import System.Taffybar.Information.Network
import System.Taffybar.Information.Battery

import System.Process (readProcessWithExitCode)
import System.IO.Unsafe (unsafePerformIO)

import Data.Function
import Data.Char (toUpper)
import Data.Either.Combinators (rightToMaybe)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.String
import Text.Read (readMaybe)
import Text.Printf (printf)
import Text.StringTemplate (newSTMP)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified GI.Gtk as Gtk
import GI.GLib (markupEscapeText)

colors :: IsString s => [s]
colors =
  [ "#121212"
  , "#ff5f5f"
  , "#afd787"
  , "#d7d7af"
  , "#87d7ff"
  , "#d7afd7"
  , "#87d7af"
  , "#d7d7d7"
  , "#121212"
  , "#ff5f5f"
  , "#afd787"
  , "#d7d7af"
  , "#87d7ff"
  , "#d7afd7"
  , "#87d7af"
  , "#d7d7d7"
  , "#000000"
  , "#d7d7d7"
  , "#87afd7"
  ]

memReader = do
  mi <- parseMeminfo
  return [memoryUsedRatio mi]

netMax = 10 * 1024 * 1024
netReader = let old = unsafePerformIO $ newIORef (-1, -1) in do
  (rx', tx') <- readIORef old
  rx:tx:_ <- fromMaybe [-1, -1] <$> getNetInfo "enp7s0"
  writeIORef old (rx, tx)
  return $ (/ netMax) . fromIntegral <$> [rx - rx', tx - tx']

batReader ctx btx = fmap (fromMaybe ([0], T.empty, [(0, 0, 0, 0)])) . runMaybeT $ do
  info <- MaybeT $ rightToMaybe <$> runReaderT (getBatteryInfo btx) ctx
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
        _ -> ((255 - 40 * val)/255, (95 + 120 * val)/255, (95 + 80 * val)/255, 1.0)
  return ([val], T.pack battTime, [color])

graphcfg colors = defaultGraphConfig
  { graphLabel = Nothing
  , graphDirection = RIGHT_TO_LEFT
  , graphDataColors = colors
  , graphBorderWidth = 2
  }
notCfg = defaultNotificationConfig
  { notificationMaxTimeout = Just 2000
  , notificationFormatter = T.replace "\n" " | " . notificationFormatter defaultNotificationConfig
  }
memCfg = graphcfg [(1, 95/255, 95/255, 1)]
cpuCfg = graphcfg [(175/255, 215/255, 135/255, 1)]
netCfg = graphcfg [(135/255, 215/255, 1, 1), (235/255, 155/255, 135/255, 0.5)]
dioCfg = graphcfg [(215/255, 215/255, 175/255, 1)]
batCfg = defaultGraphConfig
  { graphHistorySize = 60
  , graphWidth = 80
  , graphDirection = RIGHT_TO_LEFT
  , graphBorderWidth = 2
  , graphPadding = 4
  }
batOfg = defaultGraphOverlayConfig
volCfg = (defaultBarConfig $ const (0.5, 0.5, 0.5)) { barWidth = 19 }
volXfg = defaultBarXXConfig { barXXColor = (1, 95/255, 95/255), barXXBorderWidth = 2.25 }

clock = textClockNew Nothing (colorize (colors !! 3) "" "%a %b %_d %H:%M:%S") 1
workspaces = workspacesNew $ defaultWorkspacesConfig
  { maxIcons = Just 0
  , showWorkspaceFn = hideEmpty
  , underlineHeight = 0
  }
layout = betterLayoutNew defaultLayoutConfig
window = windowsNew $ defaultWindowsConfig
  { getMenuLabel = truncatedGetMenuLabel 150
  , getActiveLabel = truncatedGetActiveLabel 150
  }
note = notifyAreaNew notCfg

wea = Wea.weatherNew ((Wea.defaultWeatherConfig "KDPA") {Wea.weatherFormatter = Wea.WeatherFormatter fmt}) 10
  where fmt wi = (fmt' (Wea.tempC wi) (Wea.tempF wi) ,) . Just & fmap $ flip markupEscapeText (-1) . flip Wea.defaultWeatherFormatter wi . newSTMP . Wea.weatherTemplateTooltip . Wea.defaultWeatherConfig $ ""
        fmt' tempC tempF = "<span fgcolor='" <> weaColor tempC <> "'>" <> T.pack (show tempF) <> "°</span>"
        weaColor tempC
          | tempC > 30 = colors !! 1
          | tempC > 26 = colors !! 3
          | tempC > 15 = colors !! 2
          | otherwise  = colors !! 4
mem = pollingGraphNew memCfg 1 memReader
cpu = cpuMonitorNew cpuCfg 1 "cpu"
net = pollingGraphNew netCfg 1 netReader
dio = dioMonitorNew dioCfg 1 "sda"
bat = do
  ctx <- ask
  btxs <- getBatteryPaths
  case btxs of
    Right (btx:_) -> pollingOverlayGraphNew batCfg batOfg 1 60 (batReader ctx btx)
    _ -> do
      l <- Gtk.labelNew Nothing
      Gtk.toWidget l
vol = volumeMonitorNew volCfg volXfg

tray = sniTrayNew

main = do
  simpleTaffybar $ defaultSimpleTaffyConfig
    { startWidgets = [workspaces, layout, window]
    , endWidgets = [clock, wea, bat, dio, net, mem, cpu, vol, tray, note]
    , barHeight = 40
    , SC.barPadding = 6
    }
