module Main where

import Cli.Cli
import Lib

main :: IO ()
main = do
  opts <- getOptions
  runWithOptions opts
