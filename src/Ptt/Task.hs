{-# LANGUAGE OverloadedStrings #-}
module Ptt.Task
  ( TaskMap(..)
  , Selector
  , Tasks
  , TaskName
  , Task(..)
  , taskLength
  , totalLength
  , addTask
  , deleteTask
  , deleteTasks
  , adjustTask
  , adjustTasks
  , deleteDescription
  , deleteInterval
  , renameTask
  , moveTask
  , getTask
  , getTasksForDay
  , tasksForSelector
  , emptyTaskMap
  , deleteOldTasks
  , groupByDay
  ) where

import Prelude as P
import qualified Data.Text as T
import Control.Monad
import Control.Applicative
import Data.List
import Data.Maybe
import Data.Time.Calendar
import Data.Yaml
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Ptt.Time.Interval as I
import Ptt.Time.Date

type TaskName = T.Text
type Selector = (Day, TaskName)
type Tasks = M.Map Selector Task
newtype TaskMap = Tm { getTM :: Tasks } deriving (Show, Eq)

data Task = Task
  { taskDescriptions :: [T.Text]
  , taskTimes :: [I.Interval]
  } deriving (Ord, Eq, Show)

instance ToJSON Task where
  toJSON (Task descs times) = object
    [ "descriptions" .= toJSON descs
    , "times" .= toJSON times ]

instance FromJSON Task where
  parseJSON (Object v) =
    Task <$> v .: "descriptions" <*> v .: "times"
  parseJSON _ = mzero

instance ToJSON TaskMap where
  toJSON (Tm tasks) = toJSON . M.mapKeys formatSelector $ tasks
    where formatSelector (day, name) = T.concat [formatDay day, ":", name]

instance FromJSON TaskMap where
  parseJSON v = Tm <$> do
    m1 <- parseJSON v
    m2 <- mapM convert (M.toList m1)
    return $ M.fromList m2
    where convert (s, tasks) = do
            let (ds, n) = T.break (== ':') s
                name = T.tail n
            day <- maybe mzero return (parseDay ds)
            return ((day, name), tasks)

taskLength :: Task -> Integer
taskLength = sum . map I.intervalLength . taskTimes

totalLength :: TaskMap -> Integer
totalLength = sum . map taskLength . M.elems . getTM

merge :: Task -> Task -> Task
merge (Task descs1 times1) (Task descs2 times2) =
  let descs = descs1 ++ descs2
      times = foldl' (flip I.add) times2 times1
  in Task descs times

addTask :: Selector -> Task -> TaskMap -> TaskMap
addTask selector task (Tm tasks) =
  let existingTask = M.lookup selector tasks
      taskToInsert = maybe task (merge task) existingTask
  in Tm $ M.insert selector taskToInsert tasks

deleteDescription :: Int -> Task -> Task
deleteDescription i (Task ds ts) = Task (deleteNth i ds) ts

deleteNth :: Int -> [a] -> [a]
deleteNth i xs
  | i > 0 = take (i - 1) xs ++ drop i xs
  | otherwise = xs

changeKey :: Selector -> Selector -> TaskMap -> TaskMap
changeKey selector newSelector tasks =
  let updatedTasks = do
        task <- getTask selector tasks
        return $ addTask newSelector task (deleteTask selector tasks)
  in fromMaybe tasks updatedTasks

moveTask :: Selector -> Day -> TaskMap -> TaskMap
moveTask selector day = changeKey selector (day, snd selector)

renameTask :: Selector -> TaskName -> TaskMap -> TaskMap
renameTask selector newName = changeKey selector (fst selector, newName)

deleteInterval :: I.Interval -> Task -> Task
deleteInterval i (Task ds ts) = Task ds (I.remove i ts)

getTask :: Selector -> TaskMap -> Maybe Task
getTask selector = M.lookup selector . getTM

deleteTask :: Selector -> TaskMap -> TaskMap
deleteTask selector = Tm . M.delete selector . getTM

deleteTasks :: DateSelector -> TaskName -> TaskMap -> TaskMap
deleteTasks selector name tasks =
  let foundTasks = tasksWithName name . tasksForSelector selector $ tasks
  in Tm $ M.difference (getTM tasks) foundTasks

adjustTask :: (Task -> Task) -> Selector -> TaskMap -> TaskMap
adjustTask f selector = Tm . M.adjust f selector . getTM

deleteOldTasks :: Day -> TaskMap -> TaskMap
deleteOldTasks d (Tm tasks) = Tm $ M.filterWithKey isNewer tasks
  where isNewer (k, _) _ = k >= d

getTasksForDay :: Day -> TaskMap -> [(TaskName, Task)]
getTasksForDay d = M.toList . M.mapKeys snd . tasksForDay d

tasksForDay :: Day -> TaskMap -> Tasks
tasksForDay day = M.filterWithKey f . getTM
  where f (k, _) _ = k == day

emptyTaskMap :: TaskMap
emptyTaskMap = Tm M.empty

tasksWithName :: TaskName -> Tasks -> Tasks
tasksWithName name = M.filterWithKey f
  where f (_, n) _ = n == name

tasksForSelector :: DateSelector -> TaskMap -> Tasks
tasksForSelector selector tasks =
  case selector of
    AllDates -> getTM tasks
    DateRange from to -> tasksBetweenDays from to tasks
    SingleDate day -> tasksForDay day tasks

tasksBetweenDays :: Day -> Day -> TaskMap -> Tasks
tasksBetweenDays from to = M.filterWithKey f . getTM
  where f (day, _) _ = from <= day && day <= to

adjustTasks :: (Task -> Task) -> DateSelector -> TaskMap -> TaskMap
adjustTasks f selector taskmap =
  mergeTasks taskmap . M.toList . M.map f
  . tasksForSelector selector $ taskmap

mergeTasks :: TaskMap -> [(Selector, Task)] -> TaskMap
mergeTasks taskmap = foldl' merger taskmap
  where merger tm (selector, task) = addTask selector task tm

groupByDay :: Tasks -> [(Day, (S.Set (TaskName, Task)))]
groupByDay = sort . M.toList . M.foldlWithKey folder M.empty
  where folder acc (day, name) task =
          M.insertWith S.union day (S.singleton (name, task)) acc

