{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Control.Monad.Loops
import Control.Monad
import System.Random
import System.IO

import qualified Data.Map as M
import Data.Maybe (fromMaybe)

data PlayerParity = PlayerParity
                      { parity :: Parity
                      , player :: Player
                      } deriving (Eq)

instance Show PlayerParity where
  show p =  (show. player) p ++  " is " ++ (show. parity)  p

data Config = Config
                { asParityMap :: M.Map Parity Player
                , asString :: String
                }

data Parity = Even | Odd deriving (Ord, Eq,Show)

data Player = Computer | Human | Nobody deriving (Eq, Ord, Show)

data RoundResult = RoundResult
                    { copmuter :: Int
                    , human    :: Int
                    , winner   :: Player
                    } deriving (Show)

type Trigram = ((Parity, Parity), Parity)

chooseParity :: IO Config
chooseParity = do
  choose <- randomRIO (0,1)
  return $ case choose::Int of
    0 -> Config
            (M.fromList $ [(Even, Computer), (Odd, Human)])
            ("Computer is Even, Human is Odd")
    1 -> Config
            (M.fromList $ [(Odd, Computer), (Even, Human)])
            ("Computer is Odd, Human is Even")

chooseZereOne :: IO Int
chooseZereOne = do
  randomRIO (0, 1)

singleRoundMorra :: ReaderT Config IO RoundResult
singleRoundMorra = do
  parityMap     <- asks asParityMap

  humanNumer    <- lift $ putStr "Man : " >> getLine >>= return . read
  computerNumer <- lift chooseZereOne
  lift $ putStrLn ("Com : " ++ show computerNumer)

  let currentParity = determinParity computerNumer humanNumer
      winner' = (fromMaybe Nobody) $ M.lookup currentParity parityMap

  lift $ putStrLn (" - " ++ show winner' ++ " wins")
  return $ RoundResult computerNumer humanNumer winner'

determinParity :: Int -> Int -> Parity
determinParity x y = if (x + y) `mod` 2 == 0 then Even else Odd

score :: RoundResult -> Player -> Int
score r p
  | winner r == p = 1
  | otherwise     = 0


toParity :: Int -> Parity
toParity i = if i `mod` 2 == 0 then Even else Odd

loop :: ReaderT Config (StateT ([RoundResult],[Trigram]) IO) RoundResult
loop = do
  config  <- ask
  mystate <- lift $ get
  let history  = fst mystate
      trigrams = snd mystate

  roundResult  <- lift.lift $ runReaderT singleRoundMorra config
  -- lift . lift     $ print roundResult
  lift . lift     $ putStrLn " "

  let newHistory = roundResult : history
      lastThree = toParity . human <$> takeR 3 newHistory
      first  = (lastThree !! 0)
      second = (lastThree !! 1)
      third  = (lastThree !! 2)
      newTrigram = ((first, second), third)

  lift $ if length history >= 2
    then put (newHistory, newTrigram : trigrams)
    else put (newHistory, trigrams)
  return $ roundResult


checkState :: (Monad m) => StateT ([RoundResult],[Trigram]) m Bool
checkState = do
  mystate <- get
  let history  = fst mystate
      computerScore = calcScoreForPlayer Computer history
      humanScore    = calcScoreForPlayer Human history
  return $ computerScore /= 3 && humanScore /= 3

calcWinner :: [RoundResult] -> Player
calcWinner history
  | calcScoreForPlayer Computer history > calcScoreForPlayer Human history  = Computer
  | calcScoreForPlayer Computer history < calcScoreForPlayer Human history  = Human
  | otherwise = Nobody

calcScoreForPlayer :: Player -> [RoundResult] -> Int
calcScoreForPlayer p ps = length $ filter (((==) p ) . winner) ps

takeR :: Int -> [a] -> [a]
takeR = join . (foldr (const(.tail)) id.) . drop

main :: IO ()
main = do

  hSetBuffering stdout NoBuffering

  config  <- chooseParity

  putStrLn " "
  putStrLn $ "========= Configuration ========="
  putStrLn " "
  putStrLn $ " " ++ asString config
  putStrLn " "
  putStrLn $ "================================="
  putStrLn " "

  history <- evalStateT (whileM (checkState) (runReaderT loop config)) ([],[])
  -- forM_ history print
  let finalWinner = calcWinner history

  putStrLn " "
  putStrLn $ "========== The Winner! =========="
  putStrLn " "
  putStrLn $ "            " ++  show finalWinner
  putStrLn " "
  putStrLn $ "================================="
