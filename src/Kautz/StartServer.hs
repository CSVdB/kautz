{-# LANGUAGE OverloadedStrings #-}

module Kautz.StartServer where

import Network.Socket.ByteString

import Kautz.JSONUtils
import Kautz.Neighbours
import Kautz.SeedServerInfo
import Kautz.SockAddr
import Kautz.Types

import qualified Data.Map.Strict as MS

startServer :: IO ()
startServer = do
    sock <- getSeedServerSocket
    listen sock 2
    mainLoop sock MS.empty

mainLoop :: Socket -> SockMap -> IO ()
mainLoop sock sockmap = do
    conn <- accept sock
    newmap <- runConn conn sockmap
    mainLoop sock newmap

runConn :: (Socket, SockAddr) -> SockMap -> IO SockMap
runConn (sock, _) sockmap = do
    message <- recv sock 1
    case decode message of
        Nothing -> do
            putStrLn $
                "Someone tried to connect using the following message:" ++
                show message
            pure sockmap
        Just sockAddr -> do
            newmap <-
                if MS.member sockAddr sockmap
                    then do
                        kautzString <- newKautzString sockmap
                        updateNeighbours sockAddr kautzString sockmap
                        pure $ MS.insert sockAddr kautzString sockmap
                    else do
                        putStrLn $
                            "Node " ++
                            show sockAddr ++ " tried connecting again."
                        pure sockmap
            close sock
            pure newmap
