module Kautz.JSONUtils where

import Import

import Control.Monad.IO.Class ()
import Data.Aeson as JSON
import Data.Aeson.Encode.Pretty as JSON
import qualified Data.ByteString.Lazy as LB

-- | Read a JSON file, throw an error if the file is missing or there is an
-- error in parsing.
readJSON :: (MonadIO m, FromJSON a) => Path Abs File -> m a
readJSON path = do
    contents <- liftIO $ LB.readFile $ toFilePath path
    case JSON.eitherDecode contents of
        Left decodeErr ->
            liftIO $
            die $
            unwords
                [ "Failed to read JSON file:"
                , toFilePath path
                , "with err"
                , decodeErr
                ]
        Right res -> pure res

-- | Read a JSON file, return a default file if the file is missing.
readJSONWithDefault :: (MonadIO m, FromJSON a) => a -> Path Abs File -> m a
readJSONWithDefault def path = fromMaybe def <$> readJSONWithMaybe path

-- | Read a JSON file, return 'Nothing' if the file is missing.
readJSONWithMaybe :: (MonadIO m, FromJSON a) => Path Abs File -> m (Maybe a)
readJSONWithMaybe path = liftIO $ forgivingAbsence $ readJSON path

-- | Write a JSON file, create the appropriate directories if necessary
writeJSON :: (MonadIO m, ToJSON a) => Path Abs File -> a -> m ()
writeJSON path value =
    liftIO $ do
        ensureDir $ parent path
        LB.writeFile (toFilePath path) (JSON.encodePretty value)

encode :: (ToJSON a) => a -> ByteString
encode = LB.toStrict . JSON.encode

decode :: (FromJSON a) => ByteString -> Maybe a
decode = JSON.decode . LB.fromStrict
