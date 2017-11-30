{-# LANGUAGE DeriveGeneric #-}

module Kautz.NodeInfo where

import Import

import Data.Aeson

import Kautz.SockAddr
import Kautz.Types

data NodeInfo = NodeInfo
    { address :: SockAddr
    , name :: KautzString
    } deriving (Show, Eq, Generic)

instance FromJSON NodeInfo

instance ToJSON NodeInfo
