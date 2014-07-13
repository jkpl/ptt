module Ptt.Task where

import Prelude as P
import qualified Data.Text as T
import Data.List
import Data.Maybe
import Data.Time.Calendar
import qualified Data.Map as M
import qualified Ptt.Interval as I

newtype TaskMap = Tm (M.Map Day Tasks) deriving Show
type Selector = (Day, TaskName)
type TaskName = T.Text
type Tasks = M.Map TaskName Task

data Task = Task
  { taskDescriptions :: [T.Text]
  , taskTimes :: [I.Interval] } deriving Show

taskLength :: Task -> Integer
taskLength = sum . map I.intervalLength . taskTimes

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
                        return $ add newName task tasks
  in fromMaybe tasks updatedTasks

deleteInterval :: I.Interval -> Task -> Task
deleteInterval i (Task ds ts) = Task ds (I.remove i ts)

getTask :: Selector -> TaskMap -> Maybe Task
getTask (day, name) (Tm tasks) = M.lookup day tasks >>= M.lookup name

addTask :: Selector -> Task -> TaskMap -> TaskMap
addTask (day, name) task (Tm tasks) = Tm $ M.adjust (add name task) day tasks

renameTask :: Selector -> TaskName -> TaskMap -> TaskMap
renameTask (day, name) newName (Tm tasks) = Tm $ M.adjust (rename newName name) day tasks

deleteTask :: Selector -> TaskMap -> TaskMap
deleteTask (day, name) (Tm tasks) = Tm $ M.adjust (M.delete name) day tasks

adjustTask :: (Task -> Task) -> Selector -> TaskMap -> TaskMap
adjustTask f (day, name) (Tm tasks) = Tm $ M.adjust (M.adjust f name) day tasks

moveTask :: Selector -> Day -> TaskMap -> TaskMap
moveTask selector@(_, name) to tasks = fromMaybe tasks moveTask'
  where moveTask' = do
          task <- getTask selector tasks
          return $ addTask (to, name) task (deleteTask selector tasks)

deleteOldTasks :: Day -> TaskMap -> TaskMap
deleteOldTasks d (Tm tasks) = Tm $ M.filterWithKey isNewer tasks
  where isNewer k _ = k >= d
