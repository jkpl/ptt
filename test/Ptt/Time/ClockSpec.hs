module Ptt.Time.ClockSpec where

import Test.Hspec
import Test.QuickCheck
import Ptt.Time.Clock

spec :: Spec
spec =
  describe "time parsing and formatting" $
    it "can be repeated to produce the original value" $ property $
      \(Positive seconds) -> secondsFromText (secondsToText seconds) == Just seconds

