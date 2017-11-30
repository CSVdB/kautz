{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Kautz.AddNode where

import Network.Socket.ByteString

import Control.Concurrent (forkIO)
import Control.Concurrent.STM.TChan
import Control.Monad.STM

import Kautz.Chan
import Kautz.JSONUtils
import Kautz.NodeInfo
import Kautz.SeedServerInfo
import Kautz.SockAddr

import qualified Data.Map.Strict as MS

addNode :: IO ()
addNode = do
    let addr = getAddrFromInt 0
    sock <- getSocketOnAddr addr
    sendToAddr addr seedServerAddr
    putStrLn "Connected to the server"
    chan <- newTChanIO
    cleanChannel chan
    atomically $ writeTChan chan MS.empty
    listen sock 2
    listenForever sock chan

listenForever :: Socket -> Channel -> IO ()
listenForever sock chan = do
    (conn, _) <- accept sock
    _ <- forkIO $ runConn conn chan
    listenForever sock chan

runConn :: Socket -> Channel -> IO ()
runConn conn chan = do
    message <- recv conn 1000
    duplicate <- atomically $ cloneTChan chan
    sockmap <- getLastElem duplicate
    newmap <-
        case decode message of
            Nothing -> do
                putStrLn
                    "Seed server sent something else than a neighbour node."
                pure sockmap
            Just NodeInfo {..} -> do
                putStrLn $ "adding address " ++ show address
                pure $ MS.insert address name sockmap
    atomically $ writeTChan chan newmap
    close conn
