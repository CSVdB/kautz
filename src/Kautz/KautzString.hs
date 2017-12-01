module Kautz.KautzString where

import Import

import Test.QuickCheck.Gen

type KautzString = String

noEqualNeighbours :: KautzString -> Bool
noEqualNeighbours [] = True
noEqualNeighbours [_] = True
noEqualNeighbours (x:(y:xs)) = x /= y && noEqualNeighbours (y : xs)

newKautzString :: Gen KautzString
newKautzString = do
    x1 <- elements kautzStringChars
    x2 <- elements $ delete x1 kautzStringChars
    x3 <- elements $ delete x2 kautzStringChars
    x4 <- elements $ delete x3 kautzStringChars
    x5 <- elements $ delete x4 kautzStringChars
    pure [x1, x2, x3, x4, x5]

kautzStringChars :: String
kautzStringChars = "ab"
-- kautzStringChars = "abcde"
