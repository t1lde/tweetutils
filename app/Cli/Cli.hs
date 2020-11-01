module Cli.Cli
  ( module Cli.Options
  , getOptions
  ) where

import Cli.Options
import Cli.Parse

import Options.Applicative
import Control.Monad.Error.Class()

import Data.Time.Clock
import Data.Time.LocalTime

getOptions :: IO (CliOptions UTCTime)
getOptions = do
  opts <- cliParser
  tz <- getCurrentTimeZone
  traverse (tryParseTime tz) opts

tryParseTime :: TimeZone -> ParseTimeFmt -> IO UTCTime
tryParseTime tz (ParseTimeFmt fmt pt) = do
  Right(toUTC) <- pure $ pt fmt
  return $ toUTC tz
tryParseTime _ (ParseDuration pd) = do
  time <- getZonedTime
  return $ pd time

cliParser :: IO (CliOptions ParseTimeFmt)
cliParser = customExecParser (prefs showHelpOnEmpty) $ info (parseCliOptions <**> helper) $
  fullDesc <> (header "tweetdeletecli - automatically delete old tweets")
