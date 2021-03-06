module Kautz.SeedServerInfo where

import Network.Socket
       hiding (recv, recvFrom, recvLen, send, sendTo)
import Network.Socket.ByteString

import Data.Aeson (ToJSON)

import Kautz.JSONUtils

seedServerAddr :: SockAddr
seedServerAddr = getAddrFromInt 4242

getAddrFromInt :: Int -> SockAddr
getAddrFromInt x = SockAddrInet (toEnum x) hostAddr

hostAddr :: HostAddress
hostAddr = iNADDR_ANY

getSocket :: IO Socket
getSocket = do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    pure sock

getBoundSocket :: IO Socket
getBoundSocket = do
    sock <- getSocket
    bind sock $ SockAddrInet 0 hostAddr
    pure sock

sendToAddr :: (ToJSON a) => a -> SockAddr -> IO ()
sendToAddr msg addr = do
    sock <- getSocket
    connect sock addr
    sendAll sock $ encode msg

getSeedServerSocket :: IO Socket
getSeedServerSocket = do
    sock <- getSocket
    bind sock seedServerAddr
    pure sock
