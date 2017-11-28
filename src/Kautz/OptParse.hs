module Kautz.OptParse where

import Import

import Kautz.AddNote
import Kautz.StartServer

import System.Environment

getInstructions :: IO Instructions
getInstructions = do
    args <- getArgs
    case readArgs args of
        Just x -> pure x
        Nothing -> die "Incorrect arguments"

data Instructions
    = StartServer
    | AddNote
    deriving (Show, Eq)

readArgs :: [String] -> Maybe Instructions
readArgs ["startserver"] = Just StartServer
readArgs ["addnote"] = Just AddNote
readArgs _ = Nothing
