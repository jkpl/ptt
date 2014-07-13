module Ptt.Parse where

import Prelude as P
import qualified Data.Text as T
import Text.Read
import Ptt.Interval
import Data.Time.Calendar
import Data.Time.Format
import System.Locale (defaultTimeLocale)

secondsFromText :: T.Text -> Maybe Integer
secondsFromText time =
  case T.split isTimeSeparator time of
    (hs:ms:[]) -> do
      h <- readInt hs
      m <- readInt ms
      return $ timeOfDay h m
    (hs:[]) -> do
      h <- readInt hs
      return $ timeOfDay h 0
    _ -> Nothing

isTimeSeparator :: Char -> Bool
isTimeSeparator c = any (== c) ['.', ':']

intervalFromText :: T.Text -> Maybe Interval
intervalFromText i =
  case T.split (== '-') i of
    (fromS:toS:[]) -> do
      from <- secondsFromText fromS
      to <- secondsFromText toS
      return $ interval from to
    _ -> Nothing

readInt :: T.Text -> Maybe Int
readInt = readMaybe . T.unpack

parseDay :: T.Text -> Maybe Day
parseDay s = parseTime defaultTimeLocale "%F" (T.unpack s)

