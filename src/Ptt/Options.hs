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
import Data.Maybe
import qualified Ptt.Time.Interval as I
import Ptt.Time.Clock
import Ptt.Time.Date

data Options = Options
  { optCommand :: Command
  } deriving (Eq, Show)

data Command
  = Display (Maybe T.Text) ParsedDateSelector Bool
  | Add T.Text (Maybe I.Interval) (Maybe T.Text) (Maybe Day)
  | DeleteTask T.Text ParsedDateSelector
  | DeleteDesc T.Text Int (Maybe Day)
  | DeleteInterval T.Text I.Interval (Maybe Day)
  | Rename T.Text T.Text (Maybe Day)
  | Move T.Text Day (Maybe Day)
  deriving (Eq, Show)

dateSelector :: Parser ParsedDateSelector
dateSelector = (fromMaybe DefaultDate) <$> optional opt
  where opt = nullOption
            $ long "days"
            <> short 'd'
            <> metavar "DAYS"
            <> help "Days to operate on"
            <> eitherReader dateSelectorReader

dateSelectorReader :: String -> Either String ParsedDateSelector
dateSelectorReader s =
  case parseFromText (T.pack s) of
    Just x -> Right x
    Nothing -> Left "Could not parse given date selector"

dayOption :: Parser Day
dayOption = nullOption
  $ long "day"
  <> short 'd'
  <> metavar "DAY"
  <> help "Day to operate on"
  <> eitherReader dayReader

dayReader :: String -> Either String Day
dayReader s =
  case parseDay (T.pack s) of
    Just d -> Right d
    Nothing -> Left "Could not parse given date"

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
time = argument (secondsFromText . T.pack) (metavar "TIME")

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
opts = info (Options <$> cmd)
  $ fullDesc
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
      (Display <$> optional task <*> dateSelector <*> verbose)
      (fullDesc <> progDesc "Show accumulated log")

    add = command "add" $ info
      (Add <$> task
           <*> (optional interval)
           <*> (optional description)
           <*> (optional dayOption))
      (fullDesc <> progDesc "Add new task")

    deleteTask = command "deltask" $ info
      (DeleteTask <$> task <*> dateSelector)
      (fullDesc <> progDesc "Delete task")

    deleteDesc = command "deldesc" $ info
      (DeleteDesc <$> task <*> descriptionIndex <*> (optional dayOption))
      (fullDesc <> progDesc "Delete description with given index")

    deleteInterval = command "del" $ info
      (DeleteInterval <$> task <*> interval <*> (optional dayOption))
      (fullDesc <> progDesc "Delete interval from task")

    rename = command "rename" $ info
      (Rename <$> task <*> task <*> (optional dayOption))
      (fullDesc <> progDesc "Rename task")

    move = command "move" $ info
      (Move <$> task <*> day <*> (optional dayOption))
      (fullDesc <> progDesc "Move task to other day")

getOptions :: IO Options
getOptions = customExecParser (prefs showHelpOnError) opts
