{-# LANGUAGE OverloadedStrings #-}
module Ptt.Time.Clock
  ( hours
  , minutesOfHours
  , timeOfDay
  , secondsToText
  , secondsToLength
  , secondsFromText
  ) where

import Prelude as P
import qualified Data.Text as T
import Control.Applicative
import Ptt.Util

hours :: Integral a => a -> Integer
hours seconds = fromIntegral $ seconds `div` 3600

minutesOfHours :: Integral a => a -> Int
minutesOfHours seconds = fromIntegral $ seconds `rem` 3600 `div` 60

secondsOfMinutes :: Integral a => a -> Int
secondsOfMinutes seconds = fromIntegral $ seconds `rem` 60

timeOfDay :: Integral a => a -> a -> Integer
timeOfDay hour minute =
  let hourSeconds = hour * 3600
      minuteSeconds = minute * 60
  in fromIntegral $ hourSeconds + minuteSeconds

timeOfDayWithSeconds :: Integral a => a -> a -> a -> Integer
timeOfDayWithSeconds hour minute second =
  timeOfDay hour minute + fromIntegral second

secondsToText :: Integral a => a -> T.Text
secondsToText seconds =
  let s = secondsOfMinutes seconds
      hs = pad $ hours seconds
      ms = pad $ minutesOfHours seconds
      hm = T.concat [hs, ":", ms]
  in if s == 0 then hm else T.concat [hm, ":", pad s]
  where pad i
          | i < 10 = T.append "0" (tshow (abs i))
          | otherwise = tshow i

secondsToLength :: Integral a => a -> T.Text
secondsToLength seconds =
  let h = formatNonZero (hours seconds) "h"
      m = formatNonZero (minutesOfHours seconds) "m"
  in T.intercalate " " $ filter (not . T.null) [h, m]
  where formatNonZero i s = if i > 0 then T.append (tshow i) s else ""

secondsFromText :: T.Text -> Maybe Integer
secondsFromText time =
  case T.split isTimeSeparator time of
    (hs:ms:ss:[]) ->
      timeOfDayWithSeconds <$> readInt hs <*> readInt ms <*> readInt ss
    (hs:ms:[]) -> timeOfDay <$> readInt hs <*> readInt ms
    (hs:[]) -> timeOfDay <$> readInt hs <*> pure 0
    _ -> Nothing

isTimeSeparator :: Char -> Bool
isTimeSeparator c = c `elem` ".:"

