module Kautz.Neighbours where

import Import

import Network.Socket.ByteString

import Kautz.JSONUtils
import Kautz.NodeInfo
import Kautz.SeedServerInfo
import Kautz.SockAddr
import Kautz.Types

import qualified Data.Map.Strict as MS

updateNeighbours :: SockAddr -> KautzString -> SockMap -> IO ()
updateNeighbours addr kautzname sockmap = do
    let newReceivers = getNewReceivers kautzname sockmap
    mapM_ (uncurry (sendInfo addr)) newReceivers
    let newSenders = getNewSenders kautzname sockmap
    mapM_ (notifySender addr kautzname) newSenders
  where
    notifySender newAddr kautzName senderAddr =
        sendInfo senderAddr kautzName newAddr

getNewReceivers :: KautzString -> SockMap -> [(KautzString, SockAddr)]
getNewReceivers kautzname =
    fmap swap . filter (kautzNeighbours kautzname . snd) . MS.assocs

kautzNeighbours :: KautzString -> KautzString -> Bool
kautzNeighbours [] _ = False
kautzNeighbours (_:xs) receiver =
    case reverse receiver of
        [] -> False
        (_:ys) -> xs == reverse ys

-- Refactor this into separate parts for
-- 1. obtaining and closing the socket
-- 2. JSON-ing the message
-- 3. sending the message
sendInfo :: SockAddr -> KautzString -> SockAddr -> IO ()
sendInfo infoAddr infoName receiverAddr = do
    sock <- getSocketOnAddr receiverAddr
    let message = encode $ NodeInfo infoAddr infoName
    sendAll sock message
    close sock

getNewSenders :: KautzString -> SockMap -> [SockAddr]
getNewSenders kautzname =
    fmap fst . filter (flip kautzNeighbours kautzname . snd) . MS.assocs
