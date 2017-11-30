{-# LANGUAGE DeriveGeneric #-}

module Kautz.Neighbours where

import Import

import qualified Data.Aeson as JSON
import Data.Aeson (FromJSON, ToJSON)

import Network.Socket
       hiding (recv, recvFrom, recvLen, send, sendTo)
import Network.Socket.ByteString

import Kautz.JSONUtils
import Kautz.NodeInfo
import Kautz.SeedServerInfo
import Kautz.SockAddr
import Kautz.Types

import qualified Data.Map.Strict as MS

updateNeighbours :: SockAddr -> KautzString -> SockMap -> IO ()
updateNeighbours addr name map = do
    let newReceivers = getNewReceivers name map
    mapM_ (uncurry (sendInfo addr)) newReceivers
    let newSenders = getNewSenders name map
    mapM_ (notifySender addr name) newSenders
  where
    notifySender addr name senderAddr = sendInfo senderAddr name addr

getNewReceivers :: KautzString -> SockMap -> [(KautzString, SockAddr)]
getNewReceivers name =
    fmap swap . filter (kautzNeighbours name . snd) . MS.assocs

kautzNeighbours :: KautzString -> KautzString -> Bool
kautzNeighbours [] _ = False
kautzNeighbours (x:xs) receiver =
    case reverse receiver of
        [] -> False
        (y:ys) -> xs == reverse ys

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
getNewSenders name =
    fmap fst . filter (flip kautzNeighbours name . snd) . MS.assocs
