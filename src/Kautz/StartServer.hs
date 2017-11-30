{-# LANGUAGE OverloadedStrings #-}

module Kautz.StartServer where

import Import

import Control.Concurrent

import Network.Socket
       hiding (recv, recvFrom, recvLen, send, sendTo)
import Network.Socket.ByteString

import qualified Data.Aeson as JSON

import Kautz.JSONUtils
import Kautz.Neighbours
import Kautz.SeedServerInfo
import Kautz.SockAddr
import Kautz.Types

import Data.Map.Lazy (Map)
import qualified Data.Map.Strict as MS

startServer :: IO ()
startServer = do
    sock <- getSeedServerSocket
    listen sock 2
    let map = MS.empty
    mainLoop sock map

mainLoop :: Socket -> SockMap -> IO ()
mainLoop sock map = do
    conn <- accept sock
    forkIO $ do
        map <- runConn conn map
        pure ()
    mainLoop sock map

runConn :: (Socket, SockAddr) -> SockMap -> IO SockMap
runConn (sock, _) map = do
    message <- recv sock 1
    case decode message of
        Nothing -> do
            putStrLn $
                "Someone tried to connect using the following message:" ++
                show message
            pure map
        Just sockAddr -> do
            newmap <-
                if MS.member sockAddr map
                    then do
                        kautzString <- newKautzString map
                        updateNeighbours sockAddr kautzString map
                        pure $ MS.insert sockAddr kautzString map
                    else do
                        putStrLn $
                            "Node " ++
                            show sockAddr ++ " tried connecting again."
                        pure map
            close sock
            pure newmap
