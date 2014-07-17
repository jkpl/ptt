{-# LANGUAGE OverloadedStrings #-}
module Ptt.Configuration
  ( Configuration(..)
  , readFromFile
  , getStoragePath
  , getLastKeptDay
  ) where

import Prelude as P
import qualified Data.Text as T
import Control.Monad
import Control.Applicative
import Data.Maybe
import Data.Time
import Data.Yaml
import System.Directory
import System.FilePath
import Ptt.Time.Date

defaultStorageFilename :: String
defaultStorageFilename = ".ptt_storage.yml"

configurationFilename :: String
configurationFilename = ".ptt.yml"

getConfPath :: IO (FilePath)
getConfPath = do
  home <- getHomeDirectory
  return $ combine home configurationFilename

defaultConfiguration :: Configuration
defaultConfiguration = Configuration Nothing Nothing

data Configuration = Configuration
  { keepDays :: Maybe Integer
  , storagePath :: Maybe T.Text
  } deriving (Eq, Show)

instance ToJSON Configuration where
  toJSON conf = object
    [ "keepDays" .= toJSON (keepDays conf)
    , "storagePath" .= toJSON (storagePath conf) ]

instance FromJSON Configuration where
  parseJSON (Object v) =
    Configuration <$> v .:? "keepDays"
                  <*> v .:? "storagePath"
  parseJSON _ = mzero

readFromFile :: IO Configuration
readFromFile = do
  confPath <- getConfPath
  confExists <- doesFileExist confPath
  case confExists of
    True -> do
      conf <- decodeFile confPath
      return $ fromMaybe defaultConfiguration conf
    False -> do
      writeDefaultConf
      return defaultConfiguration

writeDefaultConf :: IO ()
writeDefaultConf = do
  confPath <- getConfPath
  encodeFile confPath defaultConfiguration

getStoragePath :: Configuration -> IO FilePath
getStoragePath conf =
  case storagePath conf of
    Just sp -> return $ T.unpack sp
    Nothing -> do
      home <- getHomeDirectory
      return $ combine home defaultStorageFilename

getLastKeptDay :: Configuration -> IO (Maybe Day)
getLastKeptDay conf = do
  day <- currentDay
  return $ case keepDays conf of
    Nothing -> Nothing
    Just 0 -> Nothing
    Just daysToReduce -> Just $ addDays (1 - daysToReduce) day

