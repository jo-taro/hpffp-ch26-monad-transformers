{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Control.Monad
import System.Random


data Config = Config { computer :: Parity }

data Parity = Even | Odd deriving (Eq,Show)

data Player = Computer | Human deriving (Eq,Show)

data RoundResult = RoundResult
                    { copmuter :: Int
                    , human    :: Int
                    , winner   :: Player
                    } deriving (Show)

chooseParity :: IO Parity
chooseParity = do
  choose <- randomRIO (0,1)
  return $ case choose::Int of
    0 -> Even
    1 -> Odd

choosePositiveNumber :: IO Int
choosePositiveNumber = do
  randomRIO (0, maxBound)

singleRoundMorra :: ReaderT Config IO RoundResult
singleRoundMorra = do
  computer'     <- asks computer
  computerNumer <- lift choosePositiveNumber
  humanNumer    <- lift $ getLine >>= return . read
  let winner' = if determinParity computerNumer humanNumer == computer'
                  then Computer
                  else Human
  return $ RoundResult computerNumer humanNumer winner'

determinParity :: Int -> Int -> Parity
determinParity x y = if (x + y) `mod` 2 == 0 then Even else Odd

score :: RoundResult -> Player -> Int
score r p
  | winner r == p = 1
  | otherwise     = 0

loop :: Parity -> (Int, Int) -> IO Player
loop parity (com, man) = do
  currentResult <- runReaderT singleRoundMorra (Config parity)
  print currentResult
  let currentComputerScore = com + score currentResult Computer
  let currentHumanScore    = man + score currentResult Human
  if currentHumanScore == 3 || currentComputerScore == 3
    then return $ winner currentResult
    else loop parity (currentComputerScore , currentHumanScore)

main :: IO ()
main = do
  parity <- chooseParity
  finalWinner <- loop parity (0, 0)
  print $ show finalWinner