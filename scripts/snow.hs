#!/usr/bin/env runhaskell
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import Data.Default (Default (..))
import Control.Applicative ((<*>))
import Control.Monad (void)
import Control.Lens (TraversableWithIndex (..))
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import Data.Maybe (fromMaybe, catMaybes)

import System.Random (randomRIO)
import Control.Concurrent (threadDelay)

import System.Console.Terminal.Size (Window(..), size)
import System.Console.ANSI

flakes="❄❅❆"
blocks="▁▂▃▄▅▆▇█"
nblocks = length blocks

getFlake :: IO Char
getFlake = (flakes !!) <$> randomRIO (0, length flakes - 1)

ouroboros :: Monad m => (a -> m a) -> a -> m a
ouroboros f = (ouroboros f =<<) . f

main :: IO ()
main = void . runMaybeT $ do
  liftIO clearScreen
  Window rows cols <- MaybeT size
  liftIO $ ouroboros (loop rows) $ replicate cols ([], 0)

loop :: Int -> [([(Int, Char)], Int)] -> IO [([(Int, Char)], Int)]
loop rows st = do
  setCursorPosition 0 0
  threadDelay 100000
  itraverse (step rows) st

step :: Int -> Int -> ([(Int, Char)], Int) -> IO ([(Int, Char)], Int)
step rows col (flakes, pile) = flip runStateT pile $ do
  r <- liftIO $ randomRIO (1, 80::Int)
  flakes' <- if r == 1
    then do
      fl <- liftIO getFlake
      return $ (-1, fl) : flakes
    else return flakes
  catMaybes <$> traverse (step' rows col) flakes'

step' :: Int -> Int -> (Int, Char) -> StateT Int IO (Maybe (Int, Char))
step' rows col (row, flake) = do
  liftIO $ do
    setCursorPosition row col
    putChar ' '
    setCursorPosition (row+1) col
  pile <- get
  let (q, r) = quotRem pile nblocks
  if row + q + 2 == rows
    then do
      liftIO . putChar $ blocks !! r
      put $ pile + 1
      return Nothing
    else do
      liftIO $ putChar flake
      return $ Just (row + 1, flake)
