module Kautz.Types
    ( module Kautz.Types
    ) where

import Import

import Network.Socket
       hiding (recv, recvFrom, recvLen, send, sendTo)

import Test.QuickCheck.Gen

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as MS

import Network.Socket

type Msg = ByteString

type SockMap = Map SockAddr KautzString

type KautzString = String

newKautzString :: SockMap -> IO KautzString
newKautzString map = do
    string <- generate $ vectorOf 5 $ elements ['a', 'b', 'c', 'd', 'e']
    if string `elem` MS.elems map
        then newKautzString map
        else pure string

type NeighbourMap = Map SockAddr KautzString
