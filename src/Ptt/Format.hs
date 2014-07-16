{-# LANGUAGE OverloadedStrings #-}
module Ptt.Format
  ( formatTaskMap
  , formatTaskMapShort
  , formatTasks
  , formatTasksShort
  , formatTask
  , formatTaskShort
  ) where

import Prelude as P
import qualified Data.Text as T
import qualified Data.Map as M
import Ptt.Util
import Ptt.Task
import Ptt.Time.Interval
import Ptt.Time.Clock

formatTaskMap :: TaskMap -> T.Text
formatTaskMap = separateBy 2 . map formatPair . taskMapToList
  where formatPair (d, tasks) = T.concat
              [ formatDay d, ":\n"
              , indent 2 $ formatTasks tasks]

formatTaskMapShort :: TaskMap -> T.Text
formatTaskMapShort = separateBy 1 . map formatPair . taskMapToList
  where formatPair (d, tasks) = T.concat
              [ formatDay d, ":\n"
              , indent 2 $ formatTasksShort tasks]

formatTasks :: Tasks -> T.Text
formatTasks tasks =
  let tasksText = separateBy 2 . map formatPair . M.toList $ tasks
      totalText = T.append "Total: " (formatTasksTotalLength tasks)
  in T.intercalate "\n\n" [tasksText, totalText]
  where formatPair (name, task) = T.concat
              [ name, ":\n"
              , indent 2 $ formatTask task]

formatTasksShort :: Tasks -> T.Text
formatTasksShort tasks =
  let tasksText = separateBy 1 . map formatPair . M.toList $ tasks
      totalText = T.append "Total: " (formatTasksTotalLength tasks)
  in T.concat [tasksText, "\n", totalText]
  where formatPair (name, task) = T.concat [name, ": ", formatTaskShort task]

formatTask :: Task -> T.Text
formatTask task = renderBlock [formatDescriptions task, formatTime task]

formatTaskShort :: Task -> T.Text
formatTaskShort = formatTaskLength

formatDescriptions :: Task -> T.Text
formatDescriptions = T.intercalate "\n" . map (T.append "- ") . taskDescriptions

formatTime :: Task -> T.Text
formatTime task =
  let intervalText = formatIntervals task
      summary = formatTaskLength task
  in if T.null intervalText
     then T.empty
     else T.concat [intervalText, "\n=> ", summary]

formatIntervals :: Task -> T.Text
formatIntervals = T.intercalate ", " . map intervalToText . taskTimes

formatTaskLength :: Task -> T.Text
formatTaskLength = secondsToLength . taskLength

formatTasksTotalLength :: Tasks -> T.Text
formatTasksTotalLength = secondsToLength . totalLength 

removeEmptyTexts :: [T.Text] -> [T.Text]
removeEmptyTexts = filter (not . T.null . T.strip)

renderBlock :: [T.Text] -> T.Text
renderBlock = T.intercalate "\n" . removeEmptyTexts

separateBy :: Int -> [T.Text] -> T.Text
separateBy i = T.intercalate (T.replicate i "\n")

indent :: Int -> T.Text -> T.Text
indent i = T.intercalate "\n" . map indentLine . T.split (== '\n')
  where indentLine line = if T.null line
                          then line
                          else T.append (T.replicate i " ") line
