{-# OPTIONS_GHC -fno-warn-orphans #-}
module Ptt.Arbitrary where

import Test.QuickCheck
import Control.Applicative
import qualified Data.Text as T
import qualified Ptt.Time.Interval as I

instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary

instance Arbitrary I.Interval where
  arbitrary = I.interval <$> choose (0, 86400) <*> choose (0, 86400)

