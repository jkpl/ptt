module Main where

import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Control.Monad
import System.Directory
import Data.Maybe
import Data.Yaml
import Ptt.Options
import Ptt.Configuration as Conf
import Ptt.Task
import Ptt.Action

main :: IO ()
main = do
  action <- getAction
  conf <- Conf.readFromFile
  tasks <- getTasks conf
  case doAction action tasks of
    Edit newTasks -> saveTasks conf newTasks
    Print text -> printText text

printText :: T.Text -> IO ()
printText s = unless (T.null s) $ TIO.putStrLn s

getAction :: IO Action
getAction = getOptions >>= fromOptions

getTasks :: Configuration -> IO TaskMap
getTasks conf = do
  path <- getStoragePath conf
  tasksExist <- doesFileExist path
  if tasksExist then
    (do tasks <- decodeFile path
        return $ fromMaybe emptyTaskMap tasks)
  else return emptyTaskMap

saveTasks :: Configuration -> TaskMap -> IO ()
saveTasks conf tasks = do
  path <- getStoragePath conf
  day <- getLastKeptDay conf
  let ts = maybe tasks (`deleteOldTasks` tasks) day
  encodeFile path ts
