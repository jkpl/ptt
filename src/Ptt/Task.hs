{-# LANGUAGE OverloadedStrings #-}
module Ptt.Task where

import Prelude as P
import qualified Data.Text as T
import Control.Monad
import Control.Applicative
import Data.List
import Data.Maybe
import Data.Time.Calendar
import Data.Yaml
import qualified Data.Map as M
import qualified Ptt.Time.Interval as I
import Ptt.Util

newtype TaskMap = Tm (M.Map Day Tasks) deriving Show
type Selector = (Day, TaskName)
type TaskName = T.Text
type Tasks = M.Map TaskName Task

data Task = Task
  { taskDescriptions :: [T.Text]
  , taskTimes :: [I.Interval]
  } deriving Show

instance ToJSON Task where
  toJSON (Task descs times) = object
    [ "descriptions" .= toJSON descs
    , "times" .= toJSON times ]

instance FromJSON Task where
  parseJSON (Object v) =
    Task <$> v .: "descriptions" <*> v .: "times"
  parseJSON _ = mzero

instance ToJSON TaskMap where
  toJSON (Tm tasks) = toJSON . M.mapKeys formatDay $ tasks

instance FromJSON TaskMap where
  parseJSON v = Tm <$> do
    m1 <- parseJSON v
    m2 <- mapM convert (M.toList m1)
    return $ M.fromList m2
    where convert (ds, tasks) = do
            d <- parseJsonDay ds
            return (d, tasks)
          parseJsonDay ds =
            case parseDay ds of
              Just d -> return d
              Nothing -> mzero

taskLength :: Task -> Integer
taskLength = sum . map I.intervalLength . taskTimes

totalLength :: Tasks -> Integer
totalLength = sum . map taskLength . M.elems

mergeTasks :: Task -> Task -> Task
mergeTasks (Task descs1 times1) (Task descs2 times2) =
  let descs = descs1 ++ descs2
      times = foldl' (flip I.add) times2 times1
  in Task descs times

add :: TaskName -> Task -> Tasks -> Tasks
add name task tasks =
  let existingTask = do foundTask <- M.lookup name tasks
                        return $ mergeTasks task foundTask
  in M.insert name (fromMaybe task existingTask) tasks

deleteDescription :: Int -> Task -> Task
deleteDescription i (Task ds ts) = Task (deleteNth i ds) ts

deleteNth :: Int -> [a] -> [a]
deleteNth i xs
  | i > 0 = take (i - 1) xs ++ drop i xs
  | otherwise = xs

rename :: TaskName -> TaskName -> Tasks -> Tasks
rename newName name tasks =
  let updatedTasks = do task <- M.lookup name tasks
                        return $ add newName task (M.delete name tasks)
  in fromMaybe tasks updatedTasks

deleteInterval :: I.Interval -> Task -> Task
deleteInterval i (Task ds ts) = Task ds (I.remove i ts)

getTask :: Selector -> TaskMap -> Maybe Task
getTask (day, name) (Tm tasks) = M.lookup day tasks >>= M.lookup name

addTask :: Selector -> Task -> TaskMap -> TaskMap
addTask (day, name) task = adjust (add name task) day

renameTask :: Selector -> TaskName -> TaskMap -> TaskMap
renameTask (day, name) newName = adjust (rename newName name) day

deleteTask :: Selector -> TaskMap -> TaskMap
deleteTask (day, name) = adjust (M.delete name) day

adjustTask :: (Task -> Task) -> Selector -> TaskMap -> TaskMap
adjustTask f (day, name) = adjust (M.adjust f name) day

moveTask :: Selector -> Day -> TaskMap -> TaskMap
moveTask selector@(_, name) to tasks = fromMaybe tasks moveTask'
  where moveTask' = do
          task <- getTask selector tasks
          return $ addTask (to, name) task (deleteTask selector tasks)

deleteOldTasks :: Day -> TaskMap -> TaskMap
deleteOldTasks d (Tm tasks) = Tm $ M.filterWithKey isNewer tasks
  where isNewer k _ = k >= d

getTasksForDay :: Day -> TaskMap -> Maybe Tasks
getTasksForDay day (Tm tasks) = M.lookup day tasks

adjust :: (Tasks -> Tasks) -> Day -> TaskMap -> TaskMap
adjust f day (Tm tasks) = Tm $ M.insertWith (\_ ts -> f ts) day (f M.empty) tasks

emptyTaskMap :: TaskMap
emptyTaskMap = Tm M.empty
