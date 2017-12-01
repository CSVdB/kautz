{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Kautz.AddNode where

import Network.Socket.ByteString

import Control.Concurrent (forkIO)
import Control.Concurrent.STM.TChan

import Kautz.Chan
import Kautz.JSONUtils
import Kautz.NodeInfo
import Kautz.SeedServerInfo
import Kautz.SockAddr

addNode :: IO ()
addNode = do
    sock <- getBoundSocket
    addr <- getSocketName sock
    sendToAddr addr seedServerAddr
    putStrLn "Connected to the server"
    chan <- newTChanIO
    cleanChannel chan
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
    case decode message of
        Nothing ->
            putStrLn "Seed server sent something else than a neighbour node."
        Just NodeInfo {..} -> do
            putStrLn $ "adding address " ++ show address
            write chan $ NodeInfo address name
    close conn
