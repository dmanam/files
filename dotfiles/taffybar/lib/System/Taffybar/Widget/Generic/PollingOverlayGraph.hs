-- | A variant of the Graph widget that automatically updates itself
-- with a callback at a fixed interval.
module System.Taffybar.Widget.Generic.PollingOverlayGraph (
  -- * Types
  OverlayGraphHandle,
  GraphConfig(..),
  GraphOverlayConfig(..),
  GraphDirection(..),
  GraphStyle(..),
  -- * Constructors and accessors
  pollingOverlayGraphNew,
  defaultGraphConfig,
  defaultGraphOverlayConfig
  ) where

import           Control.Concurrent
import qualified Control.Exception.Enclosed as E
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.Text as T
import           Data.IORef (newIORef, readIORef, modifyIORef)
import           GI.Gtk
import           System.Taffybar.Util
import           System.Taffybar.Widget.Generic.OverlayGraph

pollingOverlayGraphNew
  :: MonadIO m
  => GraphConfig -> GraphOverlayConfig -> Double -> Int -> IO ([Double], T.Text, [(Double, Double, Double, Double)]) -> m GI.Gtk.Widget
pollingOverlayGraphNew cfg ofg pollSeconds samplePeriod action = liftIO $ do
  (graphWidget, graphHandle) <- oGraphNew cfg ofg

  counter <- newIORef 0
  _ <- onWidgetRealize graphWidget $ do
       sampleThread <- foreverWithDelay pollSeconds $ do
         esample <- E.tryAny action
         case esample of
           Left _ -> return ()
           Right (sample, text, col) -> do
             count <- readIORef counter
             when (count == 0) $ oGraphAddSample graphHandle sample
             oGraphUpdateOverlay graphHandle text
             oGraphUpdateColors graphHandle col
             modifyIORef counter ((`mod` samplePeriod) . (+ 1))
       void $ onWidgetUnrealize graphWidget $ killThread sampleThread

  return graphWidget
