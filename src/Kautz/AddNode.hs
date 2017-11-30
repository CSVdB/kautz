{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Kautz.AddNode where

import Network.Socket.ByteString

import Control.Concurrent

import Kautz.JSONUtils
import Kautz.NodeInfo
import Kautz.SeedServerInfo
import Kautz.SockAddr
import Kautz.Types

import qualified Data.Map.Strict as MS

addNode :: IO ()
addNode = do
    sock <- getSocket
    let addr = getAddrFromInt 0
    bind sock addr
    connectToServer addr
    putStrLn "connected to server"
    _ <- forkIO $ listenForever sock MS.empty
    putStrLn "I'm listening now"

listenForever :: Socket -> NeighbourMap -> IO ()
listenForever sock sockmap = do
    listen sock 2
    (conn, _) <- accept sock
    message <- recv conn 1000
    newmap <-
        case decode message of
            Nothing -> do
                putStrLn
                    "Seed server sent something else than a neighbour node."
                pure sockmap
            Just NodeInfo {..} -> pure $ MS.insert address name sockmap
    close conn
    listenForever sock newmap

connectToServer :: SockAddr -> IO ()
connectToServer addr = do
    sock <- getSeedServerSocket
    sendAll sock $ encode addr
