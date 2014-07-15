module Ptt.Time.IntervalSpec where

import Test.Hspec
import Test.QuickCheck
import Data.List
import qualified Ptt.Time.Interval as I
import Ptt.Arbitrary()

spec :: Spec
spec = do
  describe "interval" $ do
    it "selects the correct order for from and to values" $ property $
      \(I.Interval from to) -> from <= to

  describe "hasOverlap" $ do
    it "detects overlap in intervals where one is inside the other" $ do
      let i1 = I.fromTimeOfDay (8, 0) (14, 0)
          i2 = I.fromTimeOfDay (9, 30) (13, 45)
      I.hasOverlap i1 i2 `shouldBe` True
      I.hasOverlap i2 i1 `shouldBe` True
    it "detects overlap when intervals partially overlap" $ do
      let i1 = I.fromTimeOfDay (10, 23) (11, 11)
          i2 = I.fromTimeOfDay (10, 30) (19, 00)
      I.hasOverlap i1 i2 `shouldBe` True
      I.hasOverlap i2 i1 `shouldBe` True
    it "detects no overlap when intervals don't overlap" $ do
      let i1 = I.fromTimeOfDay (2, 22) (9, 19)
          i2 = I.fromTimeOfDay (18, 30) (21, 20)
      I.hasOverlap i1 i2 `shouldBe` False
      I.hasOverlap i2 i1 `shouldBe` False

  describe "intervalLength" $ do
    it "selects the length of the interval" $ property $
      \i@(I.Interval from to) -> to - from == I.intervalLength i

  describe "add" $ do
    it "creates a list of intervals that's at least as large as the list before it" $ property $
      \i i2 i3 i4 ->
        let is = intervalList [i2, i3, i4]
            nextIs = I.add i is
        in totalLength nextIs >= totalLength is

  describe "difference" $ do
    it "produces no intervals if the parameters are the same" $ property $
      \i -> null $ I.difference i i
    it "produces original interval when parameters are completeley apart" $ do
      let i1 = I.fromTimeOfDay (10, 23) (11, 11)
          i2 = I.fromTimeOfDay (18, 30) (19, 00)
      I.difference i1 i2 `shouldBe` [i1]
    it "produces no intervals when target interval is inside the removed interval" $ do
      let i1 = I.fromTimeOfDay (10, 23) (11, 11)
          i2 = I.fromTimeOfDay (8, 30) (19, 00)
      I.difference i1 i2 `shouldBe` []
    it "produces two intervals when removed interval is inside the target interval" $ do
      let i1 = I.fromTimeOfDay (10, 30) (12, 00)
          i2 = I.fromTimeOfDay (11, 00) (11, 30)
          expected1 = I.fromTimeOfDay (10, 30) (11, 00)
          expected2 = I.fromTimeOfDay (11, 30) (12, 00)
      I.difference i1 i2 `shouldBe` [expected1, expected2]
    it "produces one interval when the intervals partially overlap" $ do
      let i1 = I.fromTimeOfDay (10, 30) (12, 00)
          i2 = I.fromTimeOfDay (11, 33) (13, 30)
          i3 = I.fromTimeOfDay (8, 12) (10, 45)
      I.difference i1 i2 `shouldBe` [I.fromTimeOfDay (10, 30) (11, 33)]
      I.difference i1 i3 `shouldBe` [I.fromTimeOfDay (10, 45) (12, 00)]

  describe "remove" $ do
    it "creates a list of intervals that's never larger than a list before it" $ property $
      \i i2 i3 i4 ->
        let is = intervalList [i2, i3, i4]
            nextIs = I.remove i is
        in totalLength nextIs <= totalLength is

  describe "parsing and formatting" $ do
    it "can be repeated to produce the original interval" $ property $
      \interval -> I.intervalFromText (I.intervalToText interval) == Just interval

totalLength :: [I.Interval] -> Integer
totalLength = sum . map I.intervalLength

intervalList :: [I.Interval] -> [I.Interval]
intervalList = foldl' (flip I.add) []
