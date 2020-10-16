module System.Taffybar.Widget.Volume (
  BarConfig(..),
  BarXXConfig(..),
  BarDirection(..),
  volumeMonitorNew,
  defaultBarConfig,
  defaultBarXXConfig
  ) where

import Data.IORef
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import System.Exit

import qualified GI.Gtk

import Sound.Pulse
import Sound.Pulse.Context
import Sound.Pulse.Mainloop.Simple
import Sound.Pulse.Serverinfo
import Sound.Pulse.Sinkinfo
import Sound.Pulse.Subscribe
import Sound.Pulse.Volume
import Data.Word (Word32)

import System.Taffybar.Widget.Util
import System.Taffybar.Widget.Generic.VerticalBarX

mean :: [Double] -> Double
mean = uncurry (/) . foldl mean' (0, 0)
  where mean' (tot, len) next = (tot + next, len + 1)

contextBlock :: Context -> IO ()
contextBlock ctx = do
  state <- getContextState ctx
  case state of
    ContextReady -> return ()
    ContextFailed -> do
      err <- getContextErrStr ctx
      die $ "failed to connect to pulseaudio: " ++ err
    _ -> threadDelay 250000 *> contextBlock ctx

updateBar :: Context -> VerticalBarXHandle -> IO ()
updateBar ctx h = runPulse_ ctx $ do
  sink <- defaultSinkName <$> getServerInfoM
  info <- getContextSinkByNameM sink
  let vol = mean . cVolumeToLinear . siVolume $ info
  liftIO $ verticalBarXSetState h (vol, siMute info)

eventCb :: Context -> VerticalBarXHandle -> (SubscriptionEventFacility, SubscriptionEventType) -> Word32 -> IO ()
eventCb ctx h _ _ = updateBar ctx h

volumeMonitorNew :: MonadIO m => BarConfig -> BarXXConfig -> m GI.Gtk.Widget
volumeMonitorNew cfg xfg = liftIO $ do
  mainloop <- getMainloopImpl
  backgroundLoop $ doLoop mainloop
  ctx <- getContext mainloop "taffybar"
  _ <- connectContext ctx Nothing [ContextNoautospawn]
  (drawArea, h) <- verticalBarXNew cfg xfg
  needsSetup <- newIORef True
  _ <- GI.Gtk.onWidgetRealize drawArea . (readIORef needsSetup >>=) . flip when $ do
      contextBlock ctx
      updateBar ctx h
      _ <- subscribeEvents ctx [SubscriptionMaskSink, SubscriptionMaskServer] (eventCb ctx h)
      writeIORef needsSetup False
  return drawArea
