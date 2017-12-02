module Kautz.KautzString where

import Import

import Test.QuickCheck.Gen

type KautzString = String

newKautzString :: Int -> Int -> Gen KautzString
newKautzString nChars kautzLength = do
    let alph = kautzStringChars nChars
    addToKautzStringGen alph kautzLength $ pure []

addToKautzStringGen :: String -> Int -> Gen String -> Gen String
addToKautzStringGen _ 0 gen = gen
addToKautzStringGen alph n gen = do
    s <- gen
    nextChar <-
        case s of
            "" -> elements alph
            (x:_) -> elements $ delete x alph
    addToKautzStringGen alph (n - 1) . pure $ nextChar : s

kautzStringChars :: Int -> String
kautzStringChars nChars = getMoreKautzChars nChars []

getMoreKautzChars :: Int -> String -> String
getMoreKautzChars 0 x = x
getMoreKautzChars n [] = getMoreKautzChars (n - 1) "a"
getMoreKautzChars n (x:xs) =
    getMoreKautzChars (n - 1) $ getNextChar x : (x : xs)

getNextChar :: Char -> Char
getNextChar c = chr $ ord c + 1
