{-# LANGUAGE OverloadedStrings #-}

module Kautz.SockAddr where

import Import

import Network.Socket

import Data.Aeson

import qualified Data.Text as T

instance FromJSON SockAddr where
    parseJSON =
        withObject "SockAddr" $ \v ->
            SockAddrInet <$> v .: "port" <*> v .: "host"

instance ToJSON SockAddr where
    toJSON (SockAddrInet port host) = object ["port" .= port, "host" .= host]
    toJSON _ = error "Only SockAddrInet can be turned into JSON"

instance ToJSON PortNumber where
    toJSON port = toJSON $ fromEnum port

instance FromJSON PortNumber where
    parseJSON x = toEnum <$> parseJSON x
