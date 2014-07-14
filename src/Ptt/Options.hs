module Ptt.Options
  ( Options(..)
  , Command(..)
  , getOptions ) where

import Prelude as P
import qualified Data.Text as T
import Control.Applicative
import Options.Applicative
import Text.Read
import Data.Time.Calendar
import qualified Ptt.Interval as I
import Ptt.Util

data Options = Options
  { optDay :: Maybe Day
  , optCommand :: Command
  } deriving (Eq, Show)

data Command
  = Display (Maybe T.Text) Bool
  | Add T.Text (Maybe I.Interval) (Maybe T.Text)
  | DeleteTask T.Text
  | DeleteDesc T.Text Int
  | DeleteInterval T.Text I.Interval
  | Rename T.Text T.Text
  | Move T.Text Day
  deriving (Eq, Show)

dayOpt :: Parser (Maybe Day)
dayOpt = fmap (>>= parseDay . T.pack) . optional . strOption
  $ long "day"
  <> short 'd'
  <> metavar "DAY"
  <> help "Day to operate on"

verbose :: Parser Bool
verbose = switch
  $ long "verbose"
  <> short 'v'
  <> help "Enable verbose mode"

day :: Parser Day
day = argument (parseDay . T.pack) (metavar "DAY")

interval :: Parser I.Interval
interval = I.interval <$> time <*> time

time :: Parser Integer
time = argument (I.secondsFromText . T.pack) (metavar "TIME")

task :: Parser T.Text
task = argument (return . T.pack) (metavar "TASK")

description :: Parser T.Text
description = fmap T.pack . strOption
  $ long "description"
  <> short 'm'
  <> metavar "DESCRIPTION"
  <> help "Description for task"

descriptionIndex :: Parser Int
descriptionIndex = argument readMaybe (metavar "INDEX")

opts :: ParserInfo Options
opts = info (Options <$> dayOpt <*> cmd)
  $ fullDesc
  <> progDesc "Manage personal tasks"
  <> header "ptt - personal task tracker"

cmd :: Parser Command
cmd = subparser
  $ display
  <> add
  <> deleteTask
  <> deleteDesc
  <> deleteInterval
  <> rename
  <> move
  where
    display = command "show" $ info
      (Display <$> optional task <*> verbose)
      (fullDesc <> progDesc "Show accumulated log")

    add = command "add" $ info
      (Add <$> task
           <*> (optional interval)
           <*> (optional description))
      (fullDesc <> progDesc "Add new task")

    deleteTask = command "deltask" $ info
      (DeleteTask <$> task) (fullDesc <> progDesc "Delete task")

    deleteDesc = command "deldesc" $ info
      (DeleteDesc <$> task <*> descriptionIndex)
      (fullDesc <> progDesc "Delete description with given index")

    deleteInterval = command "del" $ info
      (DeleteInterval <$> task <*> interval)
      (fullDesc <> progDesc "Delete interval from task")

    rename = command "rename" $ info
      (Rename <$> task <*> task)
      (fullDesc <> progDesc "Rename task")

    move = command "move" $ info
      (Move <$> task <*> day)
      (fullDesc <> progDesc "Move task to other day")

getOptions :: IO Options
getOptions = execParser opts
