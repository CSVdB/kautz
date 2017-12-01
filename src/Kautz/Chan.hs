module Kautz.Chan where

import Import

import Control.Concurrent.STM.TChan
import Control.Monad.STM

import Kautz.NodeInfo

type Channel = TChan NodeInfo

cleanChannel :: Channel -> IO ()
cleanChannel chan = void $ readTheRest chan []

readEverything :: Channel -> IO [NodeInfo]
readEverything chan = do
    nodeInfos <- readTheRest chan []
    mapM_ (write chan) nodeInfos
    pure nodeInfos

readTheRest :: Channel -> [NodeInfo] -> IO [NodeInfo]
readTheRest chan nodeinfos = do
    empty <- atomically $ isEmptyTChan chan
    if empty
        then pure nodeinfos
        else do
            newNode <- atomically $ readTChan chan
            readTheRest chan (newNode : nodeinfos)

write :: Channel -> NodeInfo -> IO ()
write chan nodeInfo = atomically $ writeTChan chan nodeInfo
