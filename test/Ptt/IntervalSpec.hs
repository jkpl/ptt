{-# OPTIONS_GHC -fno-warn-orphans #-}
module Ptt.IntervalSpec where

import Control.Applicative
import Test.Hspec
import Test.QuickCheck
import Data.List
import qualified Ptt.Interval as I

instance Arbitrary I.Interval where
  arbitrary = I.interval <$> choose (0, 86400) <*> choose (0, 86400)

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

  describe "extend" $ do
    it "creates an interval that's atleast as large as larger parameter" $ property $
      \i1 i2 -> let i = I.extend i1 i2
                in I.intervalLength i >= (I.intervalLength i1) `max` (I.intervalLength i2)

  describe "add" $ do
    it "creates a list of intervals that's atleast as large as the list before it" $ property $
      \i i2 i3 i4 ->
        let is = foldl' (flip I.add) [] [i2, i3, i4]
            nextIs = I.add i is
        in totalLength nextIs >= totalLength is

totalLength :: [I.Interval] -> Integer
totalLength = sum . map I.intervalLength
