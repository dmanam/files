-- | Like the vertical bar, but this widget automatically updates
-- itself with a callback at fixed intervals.
module System.Taffybar.Widget.Generic.PollingBarX (
  -- * Types
  VerticalBarXHandle,
  BarConfig(..),
  BarXXConfig(..),
  BarDirection(..),
  -- * Constructors and accessors
  pollingBarXNew,
  verticalBarXFromCallback,
  defaultBarConfig,
  defaultBarXXConfig
  ) where

import Control.Concurrent
import Control.Exception.Enclosed ( tryAny )
import qualified GI.Gtk
import System.Taffybar.Widget.Util ( backgroundLoop )
import Control.Monad.IO.Class

import System.Taffybar.Widget.Generic.VerticalBarX

verticalBarXFromCallback :: MonadIO m
                        => BarConfig -> BarXXConfig -> IO (Double, Bool) -> m GI.Gtk.Widget
verticalBarXFromCallback cfg xfg action = liftIO $ do
  (drawArea, h) <- verticalBarXNew cfg xfg
  _ <- GI.Gtk.onWidgetRealize drawArea $ backgroundLoop $ do
      esample <- tryAny action
      traverse (verticalBarXSetState h) esample
  return drawArea

pollingBarXNew :: MonadIO m
              => BarConfig -> BarXXConfig -> Double -> IO (Double, Bool) -> m GI.Gtk.Widget
pollingBarXNew cfg xfg pollSeconds action =
  liftIO $
  verticalBarXFromCallback cfg xfg $ action <* delay
  where delay = threadDelay $ floor (pollSeconds * 1000000)
