{-# LANGUAGE OverloadedStrings #-}
module Ptt.Format
  ( formatTasksByDay
  , formatTasksByDayLong
  , formatTasksByDayShort
  , formatTasks
  , formatTasksLong
  , formatTasksShort
  , formatTask
  , formatTaskLong
  , formatTaskShort
  ) where

import Prelude as P
import qualified Data.Text as T
import qualified Data.Set as S
import Ptt.Task
import Ptt.Time

formatTasksByDay :: Bool -> Tasks -> T.Text
formatTasksByDay verbose =
  if verbose then formatTasksByDayLong else formatTasksByDayShort

formatTasksByDayLong :: Tasks -> T.Text
formatTasksByDayLong = separateBy 2 . map formatPair . groupByDay
  where formatPair (d, tasks) = T.concat
              [ formatDay d, ":\n"
              , indent 2 $ formatTasksLong (S.toList tasks)]

formatTasksByDayShort :: Tasks -> T.Text
formatTasksByDayShort = separateBy 1 . map formatPair . groupByDay
  where formatPair (d, tasks) = T.concat
              [ formatDay d, ":\n"
              , indent 2 $ formatTasksShort (S.toList tasks)]

formatTasks :: Bool -> [(TaskName, Task)] -> T.Text
formatTasks verbose =
  if verbose then formatTasksLong else formatTasksShort

formatTasksLong :: [(TaskName, Task)] -> T.Text
formatTasksLong tasks =
  let tasksText = separateBy 2 . map formatPair $ tasks
      totalText = T.append "Total: " (formatTasksTotalLength tasks)
  in T.intercalate "\n\n" [tasksText, totalText]
  where formatPair (name, task) = T.concat
              [ name, ":\n"
              , indent 2 $ formatTaskLong task]

formatTasksShort :: [(TaskName, Task)] -> T.Text
formatTasksShort tasks =
  let tasksText = separateBy 1 . map formatPair $ tasks
      totalText = T.append "Total: " (formatTasksTotalLength tasks)
  in T.concat [tasksText, "\n", totalText]
  where formatPair (name, task) = T.concat [name, ": ", formatTaskShort task]

formatTask :: Bool -> Task -> T.Text
formatTask verbose =
  if verbose then formatTaskLong else formatTaskShort

formatTaskLong :: Task -> T.Text
formatTaskLong task = renderBlock [formatDescriptions task, formatTime task]

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

formatTasksTotalLength :: [(a, Task)] -> T.Text
formatTasksTotalLength = secondsToLength . sum . map (taskLength . snd)

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
