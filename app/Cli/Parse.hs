module Cli.Parse where

import Data.Time.Clock
import Data.Time.Format hiding (ParseTime)
import Data.Time.LocalTime

import Data.Text

import Control.Monad.Reader
import Data.Functor.Identity

import Options.Applicative
import Data.Coerce

import Cli.Options
import Cli.ParseDuration

parseCliOptions :: Parser (CliOptions ParseTimeFmt)
parseCliOptions = CliOptions
  <$> (ApiKey @'Consumer @'Private <$>
          option str (long "consumer_private" <> metavar "CONSUMER_PRIVATE" <> help "API Consumer Private Key"))
  <*> (ApiKey @'Consumer @'Public <$>
          option str (long "consumer_public" <> metavar "CONSUMER_PUBLIC" <> help "API Consumer Public Key"))
  <*> hsubparser (authCommand <> deleteCommand)

authCommand :: Mod CommandFields (CliModeOptions ParseTimeFmt)
authCommand = command "auth" (info (pure CliAuthMode) (progDesc "Acquire access keypair via oauth pin."))

deleteCommand :: Mod CommandFields (CliModeOptions ParseTimeFmt)
deleteCommand = command "delete" (info (CliDeleteMode <$> parseDeleteOptions) (progDesc "Delete tweets"))

parseDeleteOptions :: Parser (CliDeleteOptions ParseTimeFmt)
parseDeleteOptions = CliDeleteOptions
  <$> (ApiKey @'Access @'Private <$> option str (long "access_private" <> metavar "ACCESS_PRIVATE" <> help "Account Access API Private Key"))
  <*> (ApiKey @'Access @'Public  <$> option str (long "access_public" <> metavar "ACCESS_PUBLIC" <> help "Account Access API Public Key"))
  <*> deleteModes
  <*> deleteFlags

deleteModes :: Parser (CliDeleteSelection ParseTimeFmt)
deleteModes
  =   (flag' DeleteAllMode (long "delete_all" <> help "Delete All Tweets"))
  <|> ((BeforeDurationMode) <$> option (eitherReader parseBeforeDuration)
        (long "before_duration"
         <> metavar "DURATION"
         <> help "Delete all tweets older than DURATION in the form \"%b months %w weeks %d days %h hours %m minutes\""))
  <|> (FromDateMode <$> parseParseTimeFmt)

parseParseTimeFmt :: Parser ParseTimeFmt
parseParseTimeFmt = ParseTimeFmt <$> parseTimeFormatString <*> fromDate

parseTimeFormatString :: Parser String
parseTimeFormatString = (option str (long "time_format" <> short 'f' <> metavar "FMT" <> value "%Y/%m/%d %H:%M:%S" <> help "Parse input date according to FMT."))

fromDate :: Parser (ParseTime)
fromDate = parseFromDate
  <$> option str
         (long "before_date"
          <> metavar "DATETIME"
          <> help "Delete all tweets posted before DATETIME, formatted according to FMT, or %Y/%m/%d %H:%M:%S by default.")

deleteFlags :: Parser CliPromptMode
deleteFlags
  =   (flag' CliDryRun (long "dry" <> short 'd' <> help "Run without deleting, displaying tweets that would be deleted"))
  <|> (flag' CliForce (long "force" <> short 'f' <> help "Delete tweets without prompting for confirmation"))
  <|> (flag CliConfirm CliConfirm (long "prompt" <> short 'p' <> help "Prompt for confirmation before deleting tweets (default, this flag can be ommitted)"))

parseFromDate :: String -> ParseTime
parseFromDate dateString =  \fmtString ->
  case (parseTimeM @Maybe @LocalTime False defaultTimeLocale fmtString dateString) of
    Nothing -> Left $ "Failed to parse string " <> dateString <> " according to format string " <> fmtString
    (Just localtime) -> Right $ \tz -> localTimeToUTC tz localtime

parseBeforeDuration :: String -> Either String ParseTimeFmt
parseBeforeDuration x = ParseDuration <$> parseDuration x
