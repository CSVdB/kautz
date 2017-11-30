module Kautz.OptParse where

import Import

import System.Environment

getInstructions :: IO Instructions
getInstructions = do
    args <- getArgs
    case readArgs args of
        Just x -> pure x
        Nothing -> die "Incorrect arguments"

data Instructions
    = StartServer
    | AddNode
    deriving (Show, Eq)

readArgs :: [String] -> Maybe Instructions
readArgs ["startserver"] = Just StartServer
readArgs ["addnode"] = Just AddNode
readArgs _ = Nothing
