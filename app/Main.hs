module Main where

import Control.Applicative ((<**>))
import Options.Applicative (execParser, fullDesc, header, helper, info, progDesc)

--------------------------------------------------------------------------------

import Cli.Parse      (parseCliOptions)
import TweetUtils.Lib (runWithOptions)

--------------------------------------------------------------------------------

main ∷ IO ()
main = do
  opts <- execParser $ info (parseCliOptions <**> helper )$
    fullDesc
      <> progDesc "run twitter API commands"
      <> header "tweetutils - cli utils with the Twitter API"
  runWithOptions opts
