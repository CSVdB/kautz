{-# LANGUAGE OverloadedStrings #-}

module Kautz.StartServer where

import Import

import Network.Socket.ByteString

import Control.Concurrent (forkIO)
import Control.Concurrent.STM.TChan
import Control.Monad.STM

import Kautz.Chan
import Kautz.JSONUtils
import Kautz.Neighbours
import Kautz.SeedServerInfo
import Kautz.SockAddr
import Kautz.Types

import qualified Data.Map.Strict as MS

startServer :: IO ()
startServer = do
    sock <- getSeedServerSocket
    chan <- newTChanIO
    cleanChannel chan
    listen sock 2
    mainLoop sock chan

mainLoop :: Socket -> Channel -> IO ()
mainLoop sock chan = do
    atomically $ writeTChan chan MS.empty
    empty <- atomically $ isEmptyTChan chan
    if empty
        then die "Can't write to the channel!"
        else putStrLn "everything is okay"
    conn <- accept sock
    _ <- forkIO $ runConn conn chan
    putStrLn "Accepted a connection"
    mainLoop sock chan

runConn :: (Socket, SockAddr) -> Channel -> IO ()
runConn (sock, _) chan = do
    message <- recv sock 1000
    duplicate <- atomically $ cloneTChan chan
    empty <- atomically $ isEmptyTChan chan
    if empty
        then die "Channel chan is empty!"
        else putStrLn "everything is okay"
    putStrLn "About to get the socket map"
    sockmap <- getLastElem duplicate
    print sockmap
    putStrLn "Still working"
    newmap <-
        executeIfGoodMessage message sockmap $ \(sockAddr, thismap) -> do
            putStrLn "Creating new KautzString"
            kautzString <- newKautzStringInMap thismap
            putStrLn "Created new KautzString"
            updateNeighbours sockAddr kautzString thismap
            pure $ MS.insert sockAddr kautzString thismap
    atomically $ writeTChan chan newmap
    close sock

executeIfGoodMessage ::
       ByteString
    -> SockMap
    -> ((SockAddr, SockMap) -> IO SockMap)
    -> IO SockMap
executeIfGoodMessage msg sockmap f =
    case decode msg of
        Nothing -> do
            putStrLn $
                "Someone tried to connect using the following message:" ++
                show msg
            pure sockmap
        Just sockAddr ->
            if MS.member sockAddr sockmap
                then f (sockAddr, sockmap)
                else do
                    putStrLn $
                        "Node " ++ show sockAddr ++ " tried connecting again."
                    pure sockmap
