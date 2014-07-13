module Main where

import Ptt.Options (getOptions)

main :: IO ()
main = do
  opts <- getOptions
  putStrLn $ show opts

