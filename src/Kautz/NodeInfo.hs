{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Kautz.NodeInfo where

import Import

import Test.QuickCheck.Gen

import Data.Aeson

import Kautz.KautzString
import Kautz.SockAddr

data NodeInfo = NodeInfo
    { address :: SockAddr
    , name :: KautzString
    } deriving (Show, Eq, Generic)

instance FromJSON NodeInfo

instance ToJSON NodeInfo

getAddressAndName :: NodeInfo -> (SockAddr, KautzString)
getAddressAndName NodeInfo {..} = (address, name)

newKautzStringInList :: [NodeInfo] -> IO KautzString
newKautzStringInList nodeInfos = do
    s <- generate newKautzString
    if s `elem` fmap name nodeInfos
        then newKautzStringInList nodeInfos
        else pure s
