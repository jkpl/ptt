{-# LANGUAGE OverloadedStrings #-}
module Ptt.Action
  ( Result(..)
  , action
  ) where

import Prelude as P
import qualified Data.Text as T
import Data.Maybe
import Data.Time.Calendar
import Ptt.Task
import qualified Ptt.Format as F
import Ptt.Options

data Result
  = Edit TaskMap
  | Print T.Text
  deriving Show

action :: Day -> Command -> TaskMap -> Result
action day command tasks =
  case command of
    Display taskName verbose -> Print $ showTasks day verbose taskName tasks
    _ -> Edit $ edit day command tasks

edit :: Day -> Command -> TaskMap -> TaskMap
edit day command tasks =
  case command of
    Add s interval d ->
      let selector = (day, s)
      in addTask selector (Task (maybeToList d) (maybeToList interval)) tasks
    DeleteTask s -> deleteTask (day, s) tasks
    DeleteDesc s i -> adjustTask (deleteDescription i) (day, s) tasks
    DeleteInterval s interval ->
      let selector = (day, s)
      in adjustTask (deleteInterval interval) selector tasks
    Rename s n -> renameTask (day, s) n tasks
    Move s d -> moveTask (day, s) d tasks
    _ -> tasks

showTasks :: Day -> Bool -> Maybe TaskName -> TaskMap -> T.Text
showTasks day verbose taskName tasks =
  case taskName of
    Just name -> fromMaybe T.empty $ do
      task <- getTask (day, name) tasks
      let fn = if verbose then F.formatTask else F.formatTaskShort
      return $ fn task
    Nothing ->
      let fn = if verbose then F.formatTasks else F.formatTasksShort
      in fn (getTasksForDay day tasks)
