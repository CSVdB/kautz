module Kautz.OptParse where

import Import

import Text.Read

import System.Environment

getInstructions :: IO Instructions
getInstructions = do
    args <- getArgs
    case readArgs args of
        Just x -> pure x
        Nothing -> die "Incorrect arguments"

data Instructions
    = StartServer { nOfChar :: Int
                  , lengthOfKautzString :: Int }
    | AddNode
    deriving (Show, Eq)

readArgs :: [String] -> Maybe Instructions
readArgs ["startserver", x, y] = StartServer <$> readMaybe x <*> readMaybe y
readArgs ["addnode"] = Just AddNode
readArgs _ = Nothing
