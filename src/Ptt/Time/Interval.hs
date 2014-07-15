{-# LANGUAGE OverloadedStrings #-}
module Ptt.Time.Interval
  ( Interval(..)
  , interval
  , intervalIsEmpty
  , fromTimeOfDay
  , intervalLength
  , hasOverlap
  , add
  , difference
  , remove
  , intervalToText
  , intervalFromText
  ) where

import Prelude as P
import qualified Data.Text as T
import Control.Monad
import Data.Yaml
import Data.List
import Ptt.Time.Clock

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

fromTimeOfDay :: (Integer, Integer) -> (Integer, Integer) -> Interval
fromTimeOfDay from to = interval (uncurry timeOfDay from) (uncurry timeOfDay to)

intervalIsEmpty :: Interval -> Bool
intervalIsEmpty (Interval from to) = from == to

intervalLength :: Interval -> Integer
intervalLength (Interval from to) = abs $ to - from

hasOverlap :: Interval -> Interval -> Bool
hasOverlap (Interval f1 t1) (Interval f2 t2) =
  (f1 <= f2 && f2 <= t1) || (f1 <= t2 && t2 <= t1) ||
  (f2 <= f1 && f1 <= t2) || (f2 <= t1 && t1 <= t2)

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
difference i1@(Interval f1 t1) (Interval f2 t2)
  | f1 < f2 && t2 < t1 = [interval f1 f2, interval t2 t1]
  | f2 <= f1 && t1 <= t2 = []
  | t2 < f1 || t1 < f2 = i1 : []
  | otherwise =
    let from = if f2 < f1 then max f1 t2 else f1
        to = if f2 < f1 then t1 else min t1 f2
    in interval from to : []

remove :: Interval -> [Interval] -> [Interval]
remove i = concatMap (\a -> difference a i)

intervalToText :: Interval -> T.Text
intervalToText (Interval from to) =
  let fromS = secondsToText from
      toS = secondsToText to
  in T.concat [fromS, " - ", toS]

intervalFromText :: T.Text -> Maybe Interval
intervalFromText i =
  case T.split (== '-') i of
    (fromS:toS:[]) -> do
      from <- secondsFromText fromS
      to <- secondsFromText toS
      return $ interval from to
    _ -> Nothing

