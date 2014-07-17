{-# LANGUAGE OverloadedStrings #-}
module Ptt.Time.Date
  ( ParsedDateSelector(..)
  , DateSelector(..)
  , parseFromText
  , selectorFromText
  , toSelector
  , toSelectorWithDay
  , currentDay
  , parseDay
  , formatDay
  ) where

import Prelude as P
import qualified Data.Text as T
import Control.Monad
import Control.Applicative
import Data.Time
import System.Locale (defaultTimeLocale)

data ParsedDateSelector
  = DefaultDate
  | DSelector DateSelector
  deriving (Eq, Show)

data DateSelector
  = AllDates
  | SingleDate Day
  | DateRange Day Day
  deriving (Eq, Show)

toSelector :: ParsedDateSelector -> IO (DateSelector)
toSelector pds = (flip toSelectorWithDay pds) <$> currentDay

toSelectorWithDay :: Day -> ParsedDateSelector -> DateSelector
toSelectorWithDay day pds =
  case pds of
    DefaultDate -> SingleDate day
    DSelector s -> s

parseFromText :: T.Text -> Maybe ParsedDateSelector
parseFromText s = parseDefaultDate s <|> (DSelector <$> selectorFromText s)

selectorFromText :: T.Text -> Maybe DateSelector
selectorFromText s =
  parseAllDates s
  <|> dateFromText s
  <|> dateRangeFromText s

parseDefaultDate :: T.Text -> Maybe ParsedDateSelector
parseDefaultDate s =
  if T.null s || s == "default"
  then Just DefaultDate
  else Nothing

parseAllDates :: T.Text -> Maybe DateSelector
parseAllDates s = if s == "all" then Just AllDates else Nothing

dateFromText :: T.Text -> Maybe DateSelector
dateFromText s = SingleDate <$> parseDay s

dateRangeFromText :: T.Text -> Maybe DateSelector
dateRangeFromText s =
  case T.splitOn ".." s of
    (from:to:[]) -> DateRange <$> parseDay from <*> parseDay to
    _ -> Nothing

currentDay :: IO Day
currentDay = liftM utctDay getCurrentTime

parseDay :: T.Text -> Maybe Day
parseDay s = parseTime defaultTimeLocale "%F" (T.unpack s)

formatDay :: Day -> T.Text
formatDay = T.pack . formatTime defaultTimeLocale "%F"

