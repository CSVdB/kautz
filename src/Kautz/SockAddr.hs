{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}

module Kautz.SockAddr
    ( SockAddr(..)
    , Socket(..)
    , socket
    , connect
    , bind
    , listen
    , accept
    , close
    , getSocketName
    , PortNumber(..)
    ) where

import Network.Socket
       hiding (recv, recvFrom, recvLen, send, sendTo)

import Data.Aeson

instance FromJSON SockAddr where
    parseJSON =
        withObject "SockAddr" $ \v ->
            SockAddrInet <$> v .: "port" <*> v .: "host"

instance ToJSON SockAddr where
    toJSON (SockAddrInet port host) = object ["port" .= port, "host" .= host]
    toJSON _ = error "Only SockAddrInet can be turned into JSON"

instance FromJSON PortNumber where
    parseJSON x = toEnum <$> parseJSON x

instance ToJSON PortNumber where
    toJSON port = toJSON $ fromEnum port
