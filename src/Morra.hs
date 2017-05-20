{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import System.Random

data Parity = Even | Odd deriving (Eq,Show)

chooseParity :: IO Parity
chooseParity = do
  choose <- randomRIO (0,1)
  return $ case choose::Int of
    0 -> Even
    1 -> Odd

main :: IO ()
main = do
  parity <- chooseParity
  print $ show parity