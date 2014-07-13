{-# LANGUAGE OverloadedStrings #-}
module Ptt.Interval where

import Prelude as P
import qualified Data.Text as T
import Control.Monad
import Data.Yaml
import Data.List
import Ptt.Util

data Interval = Interval
  { intervalFrom :: Integer
  , intervalTo :: Integer
  } deriving Eq

instance Show Interval where
  show = T.unpack . intervalToText

instance Ord Interval where
  compare (Interval f1 t1) (Interval f2 t2) =
    case compare f1 f2 of
      EQ -> compare t1 t2
      o -> o

instance ToJSON Interval where
  toJSON = toJSON . intervalToText

instance FromJSON Interval where
  parseJSON (String s) =
    case intervalFromText s of
      Just i -> return i
      Nothing -> mzero
  parseJSON _ = mzero

interval :: Integer -> Integer -> Interval
interval from to
  | to < from = Interval to from
  | otherwise = Interval from to

hours :: Integral a => a -> Int
hours seconds = fromIntegral $ seconds `div` 3600

minutes :: Integral a => a -> Int
minutes seconds = fromIntegral $ seconds `rem` 3600 `div` 60

timeOfDay :: Integral a => a -> a -> Integer
timeOfDay hour minute =
  let hourSeconds = hour * 3600
      minuteSeconds = minute * 60
  in fromIntegral $ hourSeconds + minuteSeconds

intervalIsEmpty :: Interval -> Bool
intervalIsEmpty (Interval from to) = from == to

intervalLength :: Interval -> Integer
intervalLength (Interval from to) = abs $ to - from

hasOverlap :: Interval -> Interval -> Bool
hasOverlap (Interval f1 t1) (Interval f2 t2) =
  (f1 <= f2 && f2 <= t1) || (f1 <= t2 && t2 <= t1) 

extend :: Interval -> Interval -> Interval
extend (Interval f1 t1) (Interval f2 t2) =
  let from = min f1 f2
      to = max t1 t2
  in interval from to

add :: Interval -> [Interval] -> [Interval]
add i is =
  let (overlapping, nonOverlapping) = partition (hasOverlap i) is
      merged = foldl' extend i overlapping
  in merged : nonOverlapping

difference :: Interval -> Interval -> [Interval]
difference i1@(Interval f1 t1) i2@(Interval f2 t2)
  | i1 == i2 = []
  | f1 < f2 && t2 < t1 = [Interval f1 f2, Interval t2 t1]
  | otherwise = interval (max f1 f2) (min t1 t2) : []

remove :: Interval -> [Interval] -> [Interval]
remove i = concatMap (\a -> difference a i)

secondsToText :: Integral a => a -> T.Text
secondsToText seconds =
  let h = pad $ hours seconds
      m = pad $ minutes seconds
  in T.concat [h, ":", m]
  where pad i
          | i < 10 = T.concat ["0", tshow i]
          | otherwise = tshow i

secondsToLength :: Integral a => a -> T.Text
secondsToLength seconds =
  let h = formatNonZero (hours seconds) "h"
      m = formatNonZero (minutes seconds) "m"
  in T.intercalate " " $ filter (not . T.null) [h, m]
  where formatNonZero i s = if i > 0 then T.append (tshow i) s else ""

intervalToText :: Interval -> T.Text
intervalToText (Interval from to) =
  let fromS = secondsToText from
      toS = secondsToText to
  in T.concat [fromS, " - ", toS]

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

