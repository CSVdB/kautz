{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}

module Kautz
    ( kautz
    ) where

import Data.Text (Text)
import qualified Data.Text.Encoding as TE

import Kautz.AddNote
import Kautz.OptParse
import Kautz.StartServer

import Import

kautz :: IO ()
kautz = do
    instructions <- getInstructions
    execute instructions

execute :: Instructions -> IO ()
execute StartServer = startServer
execute AddNote = addNote
