module Main where

import Cli.Cli

main :: IO ()
main = do
  opts <- getOptions
  putStrLn $ show opts
