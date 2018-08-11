module XMonad.Prompt.Power (powerPrompt) where

import XMonad
import XMonad.Prompt

import Control.Monad (when)

data Power = Power

instance XPrompt Power where
  showXPrompt _ = ""

actions = ["poweroff", "restart", "suspend", "hibernate"]

powerCompl = mkComplFunFromList' actions

powerPrompt conf = mkXPrompt Power conf powerCompl $ \action ->
  when (action `elem` actions) $ spawn $ "systemctl " ++ action
