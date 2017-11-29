{-# LANGUAGE OverloadedStrings #-}

module Kautz.StartServer where

import Import

import Control.Concurrent

import Network.Socket
       hiding (recv, recvFrom, recvLen, send, sendTo)
import Network.Socket.ByteString

import Kautz.Neighbours
import Kautz.SockAddr
import Kautz.Types

import Data.Map.Lazy (Map)
import qualified Data.Map.Strict as MS

startServer :: IO ()
startServer = do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet 4242 iNADDR_ANY)
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
    (_, sockAddr) <- recvFrom sock 1
    kautzString <- newKautzString map
    updateNeighbours sockAddr kautzString map
    let newmap = MS.insert sockAddr kautzString map
    close sock
    return newmap
