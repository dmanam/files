module XMonad.Prompt.Env (envPrompt) where

import XMonad
import XMonad.Prompt

import System.Environment (setEnv, getEnvironment)

import Data.List (isPrefixOf)

data EnvVar = EnvVar
newtype EnvVal = EnvVal String

instance XPrompt EnvVar where
  showXPrompt _ = "Set: "

instance XPrompt EnvVal where
  showXPrompt (EnvVal var) = "Set " ++ var ++ " to: "

envCompl :: String -> IO [String]
envCompl str = filter (str `isPrefixOf`) . map fst <$> getEnvironment

envPrompt :: XPConfig -> X ()
envPrompt conf = mkXPrompt EnvVar conf envCompl (envValPrompt conf)

envValPrompt :: XPConfig -> String -> X ()
envValPrompt conf var = mkXPrompt (EnvVal var) conf (const (return [])) (io . setEnv var)
