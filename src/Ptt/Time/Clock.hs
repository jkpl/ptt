{-# LANGUAGE OverloadedStrings #-}
module Ptt.Time.Clock
  ( hours
  , minutes
  , timeOfDay
  , secondsToText
  , secondsToLength
  , secondsFromText
  ) where

import Prelude as P
import qualified Data.Text as T
import Ptt.Util

hours :: Integral a => a -> Integer
hours seconds = fromIntegral $ seconds `div` 3600

minutes :: Integral a => a -> Int
minutes seconds = fromIntegral $ seconds `rem` 3600 `div` 60

timeOfDay :: Integral a => a -> a -> Integer
timeOfDay hour minute =
  let hourSeconds = hour * 3600
      minuteSeconds = minute * 60
  in fromIntegral $ hourSeconds + minuteSeconds

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

