module Main where

import Options.Applicative
  (execParser, info, progDesc, fullDesc, header, helper)
import Control.Applicative ((<**>))

--------------------------------------------------------------------------------

import TweetUtils.Lib (runWithOptions)
import Cli.Parse (parseCliOptions)

--------------------------------------------------------------------------------

main :: IO ()
main = do
  opts <- execParser $ info (parseCliOptions <**> helper )$
    fullDesc
      <> progDesc "run twitter API commands"
      <> header "tweetutils - cli utils with the Twitter API"
  runWithOptions opts
