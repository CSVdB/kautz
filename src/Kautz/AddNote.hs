module Kautz.AddNote where

import Import

import Network.Socket

import Kautz.SockAddr

addNote :: IO ()
addNote = do
    putStrLn "Added a note"
