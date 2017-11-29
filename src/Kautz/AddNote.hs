module Kautz.AddNote where

import Import

import Network.Socket

addNote :: IO ()
addNote = do
    putStrLn "Added a note"
