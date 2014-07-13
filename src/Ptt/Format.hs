{-# LANGUAGE OverloadedStrings #-}
module Ptt.Format where

import Prelude as P
import qualified Data.Text as T
import qualified Data.Map as M
import Ptt.Task
import Ptt.Interval

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

