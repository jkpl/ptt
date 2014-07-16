{-# LANGUAGE OverloadedStrings #-}
module Ptt.TaskSpec where

import Test.Hspec
import Test.QuickCheck
import Data.Time
import Ptt.Arbitrary()
import Ptt.Task
import qualified Ptt.Time.Interval as I

spec :: Spec
spec = do
  let task1 = Task ["desc1"] []
      task2 = Task ["desc2"] []
      task3 = Task ["desc3"] []
      day1 = fromGregorian 1968 10 1
      day2 = fromGregorian 1978 9 2
      day3 = fromGregorian 1985 7 19
      taskName1 = "taskName1"
      taskName2 = "taskName2"
      taskName3 = "taskName3"
      selector1 = (day1, taskName1)
      selector2 = (day2, taskName2)
      selector3 = (day3, taskName3)
      taskMap = addTask selector1 task1
                  (addTask selector2 task2
                    (addTask selector3 task3 emptyTaskMap))

  describe "addTask" $ do
    it "appends task when the selector doesn't match an existing task" $ do
      getTask selector1 taskMap `shouldBe` Just task1
      getTask (day1, taskName2) taskMap `shouldBe` Nothing
      getTask selector2 taskMap `shouldBe` Just task2
    it "merges tasks when the selectors match" $ do
      let i1 = I.fromTimeOfDay (8, 0) (11, 30)
          i2 = I.fromTimeOfDay (11, 0) (12, 0)
          i3 = I.fromTimeOfDay (8, 0) (12, 0)
          t1 = Task ["desc1", "desc2"] [i1]
          t2 = Task ["desc3"] [i2]
          tm = addTask selector1 t1 (addTask selector1 t2 emptyTaskMap)
          task = getTask selector1 tm
      fmap taskDescriptions task
        `shouldSatisfy` (exists (== ["desc1", "desc2", "desc3"]))
      fmap taskTimes task
        `shouldSatisfy` (exists (== [i3]))

  describe "renameTask" $ do
    it "changes the name of the task" $ do
      let tm = renameTask selector1 "zap" taskMap
      getTask (day1, "zap") tm `shouldBe` Just task1
      getTask selector1 tm `shouldBe` Nothing
      getTask selector2 tm `shouldBe` Just task2

  describe "moveTask" $ do
    it "changes the day of the task" $ do
      let d2 = fromGregorian 1972 2 3
          tm = moveTask selector1 d2 taskMap
      getTask (d2, taskName1) tm `shouldBe` Just task1
      getTask selector1 tm `shouldBe` Nothing
      getTask selector2 tm `shouldBe` Just task2

  describe "adjustTask" $ do
    it "allows modifying a found task with given function" $ do
      let interval = I.fromTimeOfDay (9, 0) (12, 0)
          descs = ["desc1"]
          tm = adjustTask (\(Task ds is) -> Task ds (interval:is)) selector1
               (addTask selector1 (Task descs []) emptyTaskMap)
          task = getTask selector1 tm
      fmap taskDescriptions task `shouldSatisfy` (exists (== descs))
      fmap taskTimes task `shouldSatisfy` (exists (== [interval]))

  describe "deleteOldTasks" $ do
    it "deletes only tasks older than the given date" $ do
      let tm1 = deleteOldTasks day1 taskMap
          tm2 = deleteOldTasks day2 taskMap
          tm3 = deleteOldTasks day3 taskMap
      getTask selector1 tm1 `shouldBe` Just task1
      getTask selector2 tm1 `shouldBe` Just task2
      getTask selector3 tm1 `shouldBe` Just task3
      getTask selector1 tm2 `shouldBe` Nothing
      getTask selector2 tm2 `shouldBe` Just task2
      getTask selector3 tm2 `shouldBe` Just task3
      getTask selector1 tm3 `shouldBe` Nothing
      getTask selector2 tm3 `shouldBe` Nothing
      getTask selector3 tm3 `shouldBe` Just task3

exists :: (a -> Bool) -> Maybe a -> Bool
exists f m = maybe False f m

