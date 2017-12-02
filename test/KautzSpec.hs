{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module KautzSpec where

import Kautz.Chan
import Kautz.NodeInfo
import Kautz.SeedServerInfo

import Control.Concurrent.STM.TChan

import Test.Hspec

spec :: Spec
spec = do
    describe "readEverything" $
        it "readEverything reads out correctly" $ do
            chan <- newTChanIO
            let node = NodeInfo seedServerAddr "aba"
            write chan node
            nodes <- readEverything chan
            nodes `shouldBe` [node]
    describe "readEverything" $
        it "readEverything leaves the channel as it was" $ do
            chan <- newTChanIO
            cleanChannel chan
            let node = NodeInfo (getAddrFromInt 0) "aba"
            write chan node
            _ <- readEverything chan
            nodes <- readEverything chan
            nodes `shouldBe` [node]
