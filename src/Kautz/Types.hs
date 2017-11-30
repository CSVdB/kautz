module Kautz.Types
    ( module Kautz.Types
    ) where

import Import

import Test.QuickCheck.Gen

import Kautz.SockAddr

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as MS

type Msg = ByteString

type SockMap = Map SockAddr KautzString

type KautzString = String

newKautzString :: SockMap -> IO KautzString
newKautzString sockmap = do
    string <- generate $ vectorOf 5 $ elements ['a', 'b', 'c', 'd', 'e']
    if string `elem` MS.elems sockmap
        then newKautzString sockmap
        else pure string

type NeighbourMap = Map SockAddr KautzString
