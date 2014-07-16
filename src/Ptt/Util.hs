{-# LANGUAGE OverloadedStrings #-}
module Ptt.Util
  ( readInt
  , tshow
  ) where

import Prelude as P
import qualified Data.Text as T
import Text.Read (readMaybe)

readInt :: T.Text -> Maybe Int
readInt = readMaybe . T.unpack

tshow :: Show a => a -> T.Text
tshow = T.pack . show

