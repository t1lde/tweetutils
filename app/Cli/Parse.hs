module Cli.Parse where

import Data.Char qualified as Char
import Control.Applicative (Alternative (..))

import TweetUtils.Options
  (AppOptions (..), AppMode (..), DumpTweetOpts (..), FormatType (..), LogLevel (..), ApiKey (..))

import Options.Applicative
  (Parser, CommandFields, Mod, ReadM,
   option, str, long, metavar, help, hsubparser, command, info, progDesc, flag, flag', short, action, completeWith)

parseCliOptions :: Parser AppOptions
parseCliOptions =
  AppOptions <$>
    ((Just . ApiKey <$> (option str (long "consumer_private" <> metavar "CONSUMER_PRIVATE" <> help "API Consumer Private Key")))
        <|> (Just . ApiKeyFile <$> (option str (long "consumer_private_file" <> metavar "path" <> help "API Consumer Private Key (file)" <> action "file")))
        <|> (pure Nothing)
    ) <*>
    ((Just . ApiKey <$> (option str (long "consumer_public" <> metavar "CONSUMER_PUBLIC" <> help "API Consumer Public Key")))
        <|> (Just . ApiKeyFile <$> (option str (long "consumer_public_file" <> metavar "path" <> help "API Consumer Public Key (file)" <> action "file")))
        <|> (pure Nothing)
    ) <*>
    ((flag' Quiet (long "quiet" <> short 'q' <> help "don't print log messages")) <|>
      (flag Normal Debug (long "verbose" <> short 'v' <> help "print more log messages"))
    ) <*>
    hsubparser (authCommand <> dumpTweetsCommand)

authCommand :: Mod CommandFields AppMode
authCommand = command "auth" (info (pure Auth) (progDesc "Acquire access keypair via oauth pin."))

--
dumpTweetsCommand :: Mod CommandFields AppMode
dumpTweetsCommand = command "dump-tweets" (info (DumpTweets <$> dumpTweetsOptions) (progDesc "Dump tweets & interactions to file"))


dumpTweetsOptions :: Parser DumpTweetOpts
dumpTweetsOptions =
  DumpTweetOpts <$>
    ((Just . ApiKey <$> (option str (long "access_private" <> metavar "ACCESS_PRIVATE" <> help "Account Access Private Key")))
        <|> (Just . ApiKeyFile <$> (option str (long "access_private_file" <> metavar "path" <> help "Account Access Private Key (file)" <> action "file")))
        <|> (pure Nothing)
    ) <*>
    ((Just . ApiKey <$> (option str (long "access_public" <> metavar "ACCESS_PUBLIC" <> help "Account Access Public Key")))
        <|> (Just . ApiKeyFile <$> (option str (long "access_public_file" <> metavar "path" <> help "Account Access Public Key (file)" <> action "file")))
        <|> (pure Nothing)
    ) <*>
    (option str (long "out_dir" <> metavar "path" <> help "output directory" <> action "directory")) <*>
    (option formatType (long "format" <> metavar "<markdown | html | org>" <> help "output file render format" <> completeWith ["markdown", "html", "org"])) <*>
    ( (flag' True (long "download_media" <> help "download embedded media files"))
        <|>  (flag True False (long "no_download_media" <> help "don't download embedded media files"))
    )

formatType :: ReadM FormatType
formatType =
  (fmap (fmap Char.toLower) $ str @String) >>= \case
    "markdown" -> pure Markdown
    "html" -> pure Html
    "org" -> pure Org
    _ -> fail "must be one of <markdown | html | org>"
--
--parseDeleteOptions :: Parser (CliDeleteOptions ParseTimeFmt)
--parseDeleteOptions = CliDeleteOptions
--  <$> (ApiKey @'Access @'Private <$> option str (long "access_private" <> metavar "ACCESS_PRIVATE" <> help "Account Access API Private Key"))
--  <*> (ApiKey @'Access @'Public  <$> option str (long "access_public" <> metavar "ACCESS_PUBLIC" <> help "Account Access API Public Key"))
--  <*> deleteModes
--  <*> deleteFlags
--  <*> deleteLikesFlags
--
--deleteModes :: Parser (CliDeleteSelection ParseTimeFmt)
--deleteModes
--  =   (flag' DeleteAllMode (long "delete_all" <> help "Delete All Tweets"))
--  <|> parseDurationStr
--  <|> (mkParseTimeFmtMode <$> parseFromDateStr <*> parseTimeFormatStr)
--
--mkParseTimeFmtMode :: String -> String -> (CliDeleteSelection ParseTimeFmt)
--mkParseTimeFmtMode datestr fmt = FromDateMode datestr (ParseTimeFmt fmt (parseFromDate datestr))
--
--parseTimeFormatStr :: Parser String
--parseTimeFormatStr = (option str (long "time_format" <> short 'f' <> metavar "FMT" <> value "%Y/%m/%d %H:%M:%S" <> help "Parse input date according to FMT."))
--
--parseFromDateStr :: Parser String
--parseFromDateStr = option str
--         (long "before_date"
--          <> metavar "DATETIME"
--          <> help "Delete all tweets posted before DATETIME, formatted according to FMT, or %Y/%m/%d %H:%M:%S by default.")
--
--parseDurationStr :: Parser (CliDeleteSelection ParseTimeFmt)
--parseDurationStr = option (eitherReader parseBeforeDuration)
--        (long "before_duration"
--         <> metavar "DURATION"
--         <> help "Delete all tweets older than DURATION in the form \"%b months %w weeks %d days %h hours %m minutes\"")
--
--deleteFlags :: Parser CliPromptMode
--deleteFlags
--  =   (flag' CliDryRun (long "dry" <> short 'd' <> help "Run without deleting, displaying tweets that would be deleted"))
--  <|> (flag' CliForce (long "force" <> short 'f' <> help "Delete tweets without prompting for confirmation"))
--  <|> (flag CliConfirm CliConfirm (long "prompt" <> short 'p' <> help "Prompt for confirmation before deleting tweets (default, this flag can be ommitted)"))
--
--parseFromDate :: String -> ParseTime
--parseFromDate dateString =  \fmtString ->
--  case (parseTimeM @Maybe @LocalTime False defaultTimeLocale fmtString dateString) of
--    Nothing -> Left $ "Failed to parse string " <> dateString <> " according to format string " <> fmtString
--    (Just localtime) -> Right $ \tz -> localTimeToUTC tz localtime
--
--parseBeforeDuration :: String -> Either String (CliDeleteSelection ParseTimeFmt)
--parseBeforeDuration x = (\p -> (BeforeDurationMode x (ParseDuration p))) <$> parseDuration x
--
--deleteLikesFlags :: Parser CliDeleteLikes
--deleteLikesFlags
--  =   (flag' CliNoDeleteLikes (long "no_likes" <> help "Don't delete likes"))
--  <|> (flag CliDeleteLikes CliDeleteLikes (long "delete_likes" <> help "Delete likes as well as tweets (default, this flag can be ommitted)"))
