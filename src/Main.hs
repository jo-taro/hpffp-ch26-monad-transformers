{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.IORef
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import System.Environment (getArgs)
import Web.Scotty.Trans

data Config = Config 
              { counts :: IORef (M.Map Text Integer)
              , prefix :: Text
              }

type Scotty  = ScottyT Text (ReaderT Config IO)
type Handler = ActionT Text (ReaderT Config IO)

bumpBoomp :: Text
          -> M.Map Text Integer
          -> (M.Map Text Integer, Integer)
bumpBoomp k m = let i = fromMaybe 1 (M.lookup k m)
                in (M.insert k (i + 1) m, i) 

app ::  Scotty ()
app =
  get "/:key" $ do
    unprefixed <- param "key"
    prefix' <- lift (asks prefix)
    let key' = mappend prefix' unprefixed
    newInteger <- lift $ do 
      counts' <- asks counts 
      lift $ do 
        m <- readIORef counts'
        let countsIntegerPair = bumpBoomp key' m
        writeIORef counts' (fst countsIntegerPair)
        return (snd countsIntegerPair)
    html $ mconcat [ "<h1> Success! Count was:"
                   , TL.pack (show newInteger)
                   , "</h1>"
                   ]

main :: IO ()
main = do
  [prefixArg] <- getArgs
  counter  <- newIORef M.empty
  let config = Config counter (TL.pack prefixArg)
      runR rma = runReaderT rma config
  scottyT 3000 runR app
