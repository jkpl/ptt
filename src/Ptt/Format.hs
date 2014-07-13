{-# LANGUAGE OverloadedStrings #-}
module Ptt.Format where

import Prelude as P
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Time.Format
import Ptt.Task
import Ptt.Interval
import System.Locale (defaultTimeLocale)
import qualified Data.Map as M

formatTasks :: Tasks -> T.Text
formatTasks = T.intercalate "\n\n" . map (formatPair) . M.toList
  where formatPair (name, task) = T.concat [name, ":\n", formatTask task]

formatTasksShort :: Tasks -> T.Text
formatTasksShort = T.intercalate "\n" . map (formatPair) . M.toList
  where formatPair (name, task) = T.concat [name, ": ", formatTaskShort task]

formatTask :: Task -> T.Text
formatTask task@(Task descs intervals) =
  let ds = T.intercalate "\n" $ map (T.append "- ") descs
      is = T.intercalate ", " $ map intervalToText intervals
      summary = formatTaskShort task
  in T.concat [ds, "\nTime: ", is, "\n=> ", summary]

formatTaskShort :: Task -> T.Text
formatTaskShort = secondsToLength . taskLength

secondsToText :: Integral a => a -> T.Text
secondsToText seconds =
  let h = pad $ hours seconds
      m = pad $ minutes seconds
  in T.concat [h, ":", m]

pad :: Int -> T.Text
pad i
  | i < 10 = T.concat ["0", tshow i]
  | otherwise = tshow i

tshow :: Show a => a -> T.Text
tshow = T.pack . show

secondsToLength :: Integral a => a -> T.Text
secondsToLength seconds =
  let h = hours seconds
      m = minutes seconds
      hs = if h > 0 then T.append (tshow h) "h" else ""
      ms = if m > 0 then T.append (tshow m) "m" else ""
  in T.intercalate " " $ filter (not . T.null) [hs, ms]

intervalToText :: Interval -> T.Text
intervalToText (Interval from to) =
  let fromS = secondsToText from
      toS = secondsToText to
  in T.concat [fromS, " - ", toS]

formatDay :: Day -> T.Text
formatDay = T.pack . formatTime defaultTimeLocale "%F"

