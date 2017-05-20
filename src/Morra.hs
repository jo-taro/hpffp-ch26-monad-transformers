{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Control.Monad.Loops
import Control.Monad
import System.Random


data Config = Config { computer :: Parity }

data Parity = Even | Odd deriving (Eq,Show)

data Player = Computer | Human | Nobody deriving (Eq,Show)

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

chooseZereOne :: IO Int
chooseZereOne = do
  randomRIO (0, 1)

singleRoundMorra :: ReaderT Config IO RoundResult
singleRoundMorra = do
  computer'     <- asks computer
  computerNumer <- lift chooseZereOne
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

loop :: Parity -> StateT (Int, Int) IO RoundResult
loop parity = do

  (com, man) <- get
  currentResult <- lift $ runReaderT singleRoundMorra (Config parity)

  lift $ print currentResult

  let currentComputerScore = com + score currentResult Computer
  let currentHumanScore    = man + score currentResult Human
  put (currentComputerScore, currentHumanScore)

  return $ currentResult


checkState :: (Monad m, Eq s, Num s) => StateT (s,s) m Bool
checkState = do
  (com, man) <- get
  return $ com /= 3 && man /= 3

calcWinner :: [RoundResult] -> Player
calcWinner history
  | calcWinner' Computer history > calcWinner' Human history  = Computer
  | calcWinner' Computer history < calcWinner' Human history  = Human
  | otherwise = Nobody
  where
    calcWinner' p ps = length $ filter (((==) p ) . winner) ps

main :: IO ()
main = do
  parity  <- chooseParity
  history <- evalStateT (whileM (checkState) (loop parity)) (0,0)
  -- forM_ history print
  let finalWinner = calcWinner history
  print $ "The Winner is : " ++ show finalWinner