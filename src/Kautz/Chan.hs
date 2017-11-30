module Kautz.Chan where

import Import

import Control.Concurrent (forkIO)
import Control.Concurrent.STM.TChan
import Control.Monad.STM

import Kautz.Types

type Channel = TChan SockMap

cleanChannel :: Channel -> IO ()
cleanChannel chan = do
    _ <-
        forkIO $
        fix $ \loop -> do
            _ <- atomically $ readTChan chan
            loop
    pure ()

getLastElem :: Channel -> IO SockMap
getLastElem chan = do
    empty <- atomically $ isEmptyTChan chan
    if empty
        then die "The channel is empty!"
        else do
            sockmap <- atomically $ readTChan chan
            emptied <- atomically $ isEmptyTChan chan
            if emptied
                then pure sockmap
                else getLastElem chan
