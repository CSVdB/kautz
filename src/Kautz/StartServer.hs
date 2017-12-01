{-# LANGUAGE OverloadedStrings #-}

module Kautz.StartServer where

import Import

import Network.Socket.ByteString

import Control.Concurrent (forkIO)
import Control.Concurrent.STM.TChan

import Kautz.Chan
import Kautz.JSONUtils
import Kautz.Neighbours
import Kautz.NodeInfo
import Kautz.SeedServerInfo
import Kautz.SockAddr

startServer :: IO ()
startServer = do
    sock <- getSeedServerSocket
    chan <- newTChanIO
    cleanChannel chan
    listen sock 2
    mainLoop sock chan

mainLoop :: Socket -> Channel -> IO ()
mainLoop sock chan = do
    conn <- accept sock
    _ <- forkIO $ runConn conn chan
    putStrLn "Accepted a connection"
    mainLoop sock chan

runConn :: (Socket, SockAddr) -> Channel -> IO ()
runConn (sock, sockAddr2) chan = do
    msg <- recv sock 1000
    write chan $ NodeInfo sockAddr2 "ba"
    nodeInfos <- readEverything chan
    case decode msg of
        Nothing -> printBadMessage msg
        Just addr ->
            if addr `elem` fmap address nodeInfos
                then printConnectAgain addr
                else do
                    kautzString <- newKautzStringInList nodeInfos
                    updateNeighbours addr kautzString nodeInfos
                    write chan $ NodeInfo addr kautzString
                    putStrLn "Wrote nodeinfo to the channel"
    close sock

printBadMessage :: ByteString -> IO ()
printBadMessage msg =
    putStrLn $
    "Someone tried to connect using the following message:" ++ show msg

printConnectAgain :: SockAddr -> IO ()
printConnectAgain addr =
    putStrLn $ "Node " ++ show addr ++ " tried connecting again."
