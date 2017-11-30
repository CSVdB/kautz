module Kautz.SeedServerInfo where

import Network.Socket
       hiding (recv, recvFrom, recvLen, send, sendTo)

seedServerAddr :: SockAddr
seedServerAddr = getAddrFromInt 4242

getAddrFromInt :: Int -> SockAddr
getAddrFromInt x = SockAddrInet (toEnum x) hostAddr

hostAddr :: HostAddress
hostAddr = iNADDR_ANY

dftFamily :: Family
dftFamily = AF_INET

dftSockType :: SocketType
dftSockType = Stream

dftProtocol :: ProtocolNumber
dftProtocol = 0

getSocket :: IO Socket
getSocket = do
    sock <- socket dftFamily dftSockType dftProtocol
    setSocketOption sock ReuseAddr 1
    pure sock

getSocketOnAddr :: SockAddr -> IO Socket
getSocketOnAddr addr = do
    sock <- getSocket
    bind sock addr
    pure sock

getSeedServerSocket :: IO Socket
getSeedServerSocket = getSocketOnAddr seedServerAddr
