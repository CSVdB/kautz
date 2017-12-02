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

startServer :: Int -> Int -> IO ()
startServer nChars kautzLength = do
    sock <- getSeedServerSocket
    chan <- newTChanIO
    cleanChannel chan
    listen sock 2
    mainLoop sock chan nChars kautzLength

mainLoop :: Socket -> Channel -> Int -> Int -> IO ()
mainLoop sock chan nChars kautzLength = do
    conn <- accept sock
    _ <- forkIO $ runConn conn chan nChars kautzLength
    putStrLn "Accepted a connection"
    mainLoop sock chan nChars kautzLength

runConn :: (Socket, SockAddr) -> Channel -> Int -> Int -> IO ()
runConn (sock, sockAddr2) chan nChars kautzLength = do
    msg <- recv sock 1000
    write chan $ NodeInfo sockAddr2 "ba"
    nodeInfos <- readEverything chan
    case decode msg of
        Nothing -> printBadMessage msg
        Just addr ->
            if addr `elem` fmap address nodeInfos
                then printConnectAgain addr
                else do
                    kautzString <-
                        newKautzStringInList nodeInfos nChars kautzLength
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
