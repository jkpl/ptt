{-# LANGUAGE OverloadedStrings #-}
module Ptt.Action
  ( Action(..)
  , Result(..)
  , doAction
  , fromOptions
  , fromOptionsWithDay
  ) where

import Prelude as P
import qualified Data.Text as T
import Control.Applicative
import Data.Maybe
import Data.Time.Calendar
import Ptt.Task
import Ptt.Time.Date
import qualified Ptt.Time.Interval as I
import qualified Ptt.Format as F
import qualified Ptt.Options as O

data Action
  = Display DateSelector (Maybe TaskName) Bool
  | Add Selector (Maybe I.Interval) (Maybe T.Text)
  | DeleteTask DateSelector TaskName
  | DeleteDesc Selector Int
  | DeleteInterval Selector I.Interval
  | Rename Selector TaskName
  | Move Selector Day
  deriving Show

data Result
  = Edit TaskMap
  | Print T.Text
  deriving Show

fromOptions :: O.Options -> IO Action
fromOptions options = (flip fromOptionsWithDay options) <$> currentDay

fromOptionsWithDay :: Day -> O.Options -> Action
fromOptionsWithDay defaultDay options =
  case O.optCommand options of
    O.Display name selector verbose ->
      Display (sel selector) name verbose
    O.Add name interval desc day ->
      Add (taskSelector day name) interval desc
    O.DeleteTask name selector ->
      DeleteTask (sel selector) name
    O.DeleteDesc name index day ->
      DeleteDesc (taskSelector day name) index
    O.DeleteInterval name interval day ->
      DeleteInterval (taskSelector day name) interval
    O.Rename name newName day ->
      Rename (taskSelector day name) newName
    O.Move name newDay day ->
      Move (taskSelector day name) newDay
  where sel selector = toSelectorWithDay defaultDay selector
        taskSelector day name = (fromMaybe defaultDay day, name)

doAction :: Action -> TaskMap -> Result
doAction action tasks =
  case action of
    Display selector taskName verbose ->
      Print $ showTasks selector taskName verbose tasks
    _ -> Edit $ edit action tasks

edit :: Action -> TaskMap -> TaskMap
edit action tasks =
  case action of
    Add selector interval desc ->
      addTask selector (Task (maybeToList desc) (maybeToList interval)) tasks
    DeleteTask selector name ->
      deleteTasks selector name tasks
    DeleteDesc selector index ->
      adjustTask (deleteDescription index) selector tasks
    DeleteInterval selector interval ->
      adjustTask (deleteInterval interval) selector tasks
    Rename selector name ->
      renameTask selector name tasks
    Move selector day ->
      moveTask selector day tasks
    _ -> tasks

showTasks :: DateSelector -> Maybe TaskName -> Bool -> TaskMap -> T.Text
showTasks selector taskName verbose tasks =
  case (selector, taskName) of
    (SingleDate day, Just name) ->
      maybe T.empty (F.formatTask verbose) (getTask (day, name) tasks)
    (SingleDate day, Nothing) ->
      F.formatTasks verbose (getTasksForDay day tasks)
    _ ->
      F.formatTasksByDay verbose (tasksForSelector selector tasks)

