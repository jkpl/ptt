module Main where

import qualified Data.Text.IO as TIO
import System.Directory
import Data.Maybe
import Data.Time
import Data.Yaml
import Ptt.Options
import Ptt.Configuration as Conf
import Ptt.Task
import Ptt.Action
import qualified Ptt.Time as Time

main :: IO ()
main = do
  opts <- getOptions
  conf <- Conf.readFromFile
  day <- getDay opts
  tasks <- getTasks conf
  let result = action day (optCommand opts) tasks
  case result of
    Edit newTasks -> saveTasks conf newTasks
    Print text -> TIO.putStrLn text

getDay :: Options -> IO Day
getDay options =
  case optDay options of
    Just d -> return d
    Nothing -> Time.currentDay

getTasks :: Configuration -> IO TaskMap
getTasks conf = do
  path <- getStoragePath conf
  tasksExist <- doesFileExist path
  case tasksExist of
    True -> do
      tasks <- decodeFile path
      return $ fromMaybe emptyTaskMap tasks
    False -> return emptyTaskMap

saveTasks :: Configuration -> TaskMap -> IO ()
saveTasks conf tasks = do
  path <- getStoragePath conf
  day <- getLastKeptDay conf
  let ts = maybe tasks (flip deleteOldTasks tasks) day
  encodeFile path ts
