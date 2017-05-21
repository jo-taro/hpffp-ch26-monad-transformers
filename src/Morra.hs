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
                , asComputerParity :: Parity
                , asString :: String
                }

data Parity = Even | Odd deriving (Ord, Eq,Show)

data Player = Computer | Human | Nobody deriving (Eq, Ord, Show)

data RoundResult = RoundResult
                    { copmuter :: Int
                    , human    :: Int
                    , winner   :: Player
                    } deriving (Show)

type TrigramKey = (Parity, Parity)
type TrigramVal = Parity

type StateType  = ([RoundResult], M.Map TrigramKey TrigramVal)


chooseParity :: IO Config
chooseParity = do
  choose <- randomRIO (0,1)
  return $ case choose::Int of
    0 -> Config
            (M.fromList $ [(Even, Computer), (Odd, Human)])
            Even
            ("Computer is Even, Human is Odd")
    1 -> Config
            (M.fromList $ [(Odd, Computer), (Even, Human)])
            Odd
            ("Computer is Odd, Human is Even")

chooseZeroOne :: IO Int
chooseZeroOne = do
  randomRIO (0, 1)


chooseComputerNumber :: Parity -> StateT StateType IO Int
chooseComputerNumber computerParity = do
  mystate <- get
  let trigrams = snd mystate
      history  = fst mystate
      lastTwo  = toParity.human <$> take 2 history
  case M.lookup (lastTwo !! 1, lastTwo !! 0) trigrams of
    Just p  ->  do
                  lift $ putStrLn "Choosing from Trigram"
                  return.toInt $ computerParityDecision computerParity p
    Nothing ->  do
                  lift $ putStrLn "Choosing from Random"
                  lift $ chooseZeroOne
    where
      computerParityDecision com man
        | com == Even && man == Even = Even
        | com == Even && man == Odd  = Odd
        | com == Odd  && man == Even = Odd
        | otherwise = Even -- com == Odd && man == Odd



singleRoundMorra :: Int -> ReaderT Config IO RoundResult
singleRoundMorra computerNumber= do
  parityMap     <- asks asParityMap

  humanNumer    <- lift $ putStr "Man : " >> getLine >>= return . read
  -- computerNumer <- lift chooseZeroOne
  lift $ putStrLn ("Com : " ++ show computerNumber)

  let currentParity = determinParity computerNumber humanNumer
      winner' = (fromMaybe Nobody) $ M.lookup currentParity parityMap

  lift $ putStrLn (" - " ++ show winner' ++ " wins")
  return $ RoundResult computerNumber humanNumer winner'

determinParity :: Int -> Int -> Parity
determinParity x y = if (x + y) `mod` 2 == 0 then Even else Odd

score :: RoundResult -> Player -> Int
score r p
  | winner r == p = 1
  | otherwise     = 0


toParity :: Int -> Parity
toParity i = if i `mod` 2 == 0 then Even else Odd

toInt :: Parity -> Int
toInt p = if p == Even then 0 else 1

loop :: ReaderT Config (StateT StateType IO) RoundResult
loop = do
  config  <- ask
  mystate <- lift $ get
  let history  = fst mystate
      trigrams = snd mystate

  computerNumber <- lift $ chooseComputerNumber (asComputerParity config)
  roundResult  <- lift.lift $ runReaderT (singleRoundMorra computerNumber) config
  -- lift . lift     $ print roundResult
  lift . lift     $ putStrLn " "

  let newHistory = roundResult : history
      lastThree = toParity . human <$> take 3 newHistory
      first  = (lastThree !! 2)
      second = (lastThree !! 1)
      third  = (lastThree !! 0)

  lift $ if length newHistory >= 3
    then put (newHistory, M.insert (first,second) third trigrams)
    else put (newHistory, trigrams)
  return $ roundResult


checkState :: (Monad m) => StateT StateType m Bool
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

  history <- evalStateT (whileM (checkState) (runReaderT loop config)) ([], M.empty)
  -- forM_ history print
  let finalWinner = calcWinner history

  putStrLn " "
  putStrLn $ "========== The Winner! =========="
  putStrLn " "
  putStrLn $ "            " ++  show finalWinner
  putStrLn " "
  putStrLn $ "================================="
