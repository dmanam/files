{-# LANGUAGE LambdaCase #-}

module System.Taffybar.Widget.Volume (
  BarConfig(..),
  BarXXConfig(..),
  BarDirection(..),
  volumeMonitorNew,
  defaultBarConfig,
  defaultBarXXConfig
  ) where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import System.Exit
import System.IO
import System.Posix.Signals
import System.Process

import qualified GI.Gtk as Gtk
import qualified GI.Gdk as Gdk

import Sound.Pulse
import Sound.Pulse.Context
import Sound.Pulse.Mainloop.Simple
import Sound.Pulse.Serverinfo
import Sound.Pulse.Sinkinfo
import Sound.Pulse.Subscribe
import Sound.Pulse.Volume
import Data.Word (Word32)

import System.Taffybar.Util
import System.Taffybar.Widget.Util
import System.Taffybar.Widget.Generic.VerticalBarX

import qualified Data.Text as T

interruptProcess :: ProcessHandle -> IO ()
interruptProcess = getPid >=> \case
  Nothing -> return ()
  Just pid -> signalProcess keyboardSignal pid

togglePavu :: IORef ProcessHandle -> IO ()
togglePavu ref = do
  ph <- readIORef ref
  exitCode <- getProcessExitCode ph
  case exitCode of
    Nothing -> interruptProcess ph
    Just _  -> writeIORef ref =<< spawnProcess "pavucontrol" []

w32mean :: [Volume] -> Double
w32mean = uncurry (/) . foldl mean' (0, 0)
  where mean' (tot, len) next = (tot + fromIntegral next / 65536, len + 1)

updateBar :: Context -> VerticalBarXHandle -> IO ()
updateBar ctx h = runPulse_ ctx $ do
  state <- liftIO $ getContextState ctx
  liftIO . guard $ state == ContextReady
  sink <- defaultSinkName <$> getServerInfoM
  info <- getContextSinkByNameM sink
  let CVolume vols = siVolume info
  liftIO $ verticalBarXSetState h (w32mean vols, siMute info)

eventCb :: Context -> VerticalBarXHandle -> (SubscriptionEventFacility, SubscriptionEventType) -> Word32 -> IO ()
eventCb ctx h _ _ = updateBar ctx h

stateCb :: Context -> IO ()
stateCb ctx = do
  state <- getContextState ctx
  case state of
    ContextFailed -> do
      err <- getContextErrStr ctx
      hPutStrLn stderr $ "pulseaudio connection failed: " ++ err
      paConnect ctx
    ContextTerminated -> paConnect ctx
    ContextUnconnected -> paConnect ctx
    _ -> return ()

paConnect :: Context -> IO ()
paConnect ctx = void $ connectContext ctx Nothing [ContextNoautospawn]

volumeMonitorNew :: MonadIO m => BarConfig -> BarXXConfig -> m Gtk.Widget
volumeMonitorNew cfg xfg = liftIO $ do
  mainloop <- getMainloopImpl
  ctx <- getContext mainloop "taffybar"
  paConnect ctx
  backgroundLoop $ doLoop mainloop
  setStateCallback ctx $ stateCb ctx
  (drawArea, h) <- verticalBarXNew cfg xfg
  needsSetup <- newIORef True
  _ <- Gtk.onWidgetRealize drawArea . (readIORef needsSetup >>=) . flip when $ do
    subscribeEvents ctx [SubscriptionMaskSink, SubscriptionMaskServer] (eventCb ctx h)
    writeIORef needsSetup False

  ebox <- Gtk.eventBoxNew
  Gtk.containerAdd ebox drawArea
  pavuRef <- newIORef =<< spawnProcess "true" []
  _ <- Gtk.onWidgetButtonPressEvent ebox $ onClick [Gdk.EventTypeButtonPress] $ togglePavu pavuRef
  Gtk.widgetShowAll ebox
  Gtk.toWidget ebox
