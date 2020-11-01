module Cli.Options where

import Data.Traversable

import Data.Time.Clock
import Data.Time.LocalTime

import Data.Text

data ApiKeyType = Consumer | Access
data ApiKeyVis  = Public   | Private

newtype ApiKey (ty :: ApiKeyType) (v :: ApiKeyVis) = ApiKey Text deriving Show via Text

type ParseTime = String -> Either String (TimeZone -> UTCTime)
type ParseDuration = (ZonedTime -> UTCTime)

data ParseTimeFmt
  = ParseTimeFmt     {parseTimeFmt :: String, runParseTime :: ParseTime }
  | ParseDuration    ParseDuration

instance Show ParseTimeFmt where
  show _ = "ParseTime ..."

data CliOptions time = CliOptions
  { cliConsumerPrivate :: ApiKey 'Consumer 'Private
  , cliConsumerPublic  :: ApiKey 'Consumer 'Public
  , cliModeOptions     :: (CliModeOptions time)
  }
  deriving (Show, Functor, Foldable, Traversable)

data CliModeOptions time
  = CliAuthMode
  | CliDeleteMode (CliDeleteOptions time)
  deriving (Show, Functor, Foldable, Traversable)

data CliDeleteOptions time = CliDeleteOptions
  { cliAccessPrivate :: ApiKey 'Access 'Private
  , cliAccessPublic  :: ApiKey 'Access 'Public
  , cliDeleteMode    :: (CliDeleteSelection time)
  , cliPromptMode    :: CliPromptMode
  }
  deriving (Show, Functor, Foldable, Traversable)

data CliDeleteSelection time
  = DeleteAllMode
  | FromDateMode time
  | BeforeDurationMode time
  deriving (Functor, Foldable, Traversable)

instance (Show a) => Show (CliDeleteSelection a) where
  show DeleteAllMode = "DeleteAllMode"
  show (FromDateMode x)  = "FromDateMode " <> show x
  show (BeforeDurationMode x) = "BeforeDurationMode " <> show x

data CliPromptMode
  = CliConfirm
  | CliForce
  | CliDryRun
  deriving (Show, Eq)
