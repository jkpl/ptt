{-# LANGUAGE OverloadedStrings #-}
module Ptt.Util
  ( parseDay
  , formatDay
  , readInt
  , tshow
  ) where

import Prelude as P
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Time.Format
import System.Locale (defaultTimeLocale)
import Text.Read (readMaybe)

parseDay :: T.Text -> Maybe Day
parseDay s = parseTime defaultTimeLocale "%F" (T.unpack s)

formatDay :: Day -> T.Text
formatDay = T.pack . formatTime defaultTimeLocale "%F"

readInt :: T.Text -> Maybe Int
readInt = readMaybe . T.unpack

tshow :: Show a => a -> T.Text
tshow = T.pack . show

