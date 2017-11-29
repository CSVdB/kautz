{-# LANGUAGE OverloadedStrings #-}

module Kautz.StartServer where

import Import

import Control.Concurrent

import Test.QuickCheck.Gen

import qualified Data.Map.Lazy as ML
import Data.Map.Lazy (Map)

import Network.Socket
       hiding (recv, recvFrom, recvLen, send, sendTo)
import Network.Socket.ByteString

startServer :: IO ()
startServer = do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet 4242 iNADDR_ANY)
    listen sock 2
    let map = ML.empty
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
    let newmap = ML.insert sockAddr kautzString map
    close sock
    return newmap

type Msg = ByteString

type SockMap = Map SockAddr KautzString

type KautzString = String

newKautzString :: SockMap -> IO KautzString
newKautzString map = do
    string <- generate $ vectorOf 5 $ elements ['a', 'b', 'c', 'd', 'e']
    if string `elem` ML.elems map
        then newKautzString map
        else pure string
