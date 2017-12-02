{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Kautz
    ( kautz
    ) where

import Kautz.AddNode
import Kautz.OptParse
import Kautz.StartServer

kautz :: IO ()
kautz = do
    instructions <- getInstructions
    execute instructions

execute :: Instructions -> IO ()
execute StartServer {..} = startServer nOfChar lengthOfKautzString
execute AddNode = addNode
