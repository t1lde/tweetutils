module TweetUtils.Options where

import Data.Traversable()

import Data.Time.Clock
import Data.Time.LocalTime

import Data.ByteString

import Web.Twitter.Conduit

data ApiKeyType = Consumer | Access
data ApiKeyVis  = Public   | Private

newtype ApiKey (ty :: ApiKeyType) (v :: ApiKeyVis) = ApiKey ByteString deriving Show via ByteString

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
  , cliDeleteLikes   :: CliDeleteLikes
  }
  deriving (Show, Functor, Foldable, Traversable)

data CliDeleteSelection time
  = DeleteAllMode
  | FromDateMode String time
  | BeforeDurationMode String time
  deriving (Functor, Foldable, Traversable)

instance (Show a) => Show (CliDeleteSelection a) where
  show DeleteAllMode = "DeleteAllMode"
  show (FromDateMode _ time)  = "FromDateMode " <> show time
  show (BeforeDurationMode _ time) = "BeforeDurationMode " <> show time

data CliPromptMode
  = CliConfirm
  | CliForce
  | CliDryRun
  deriving (Show, Eq)

data CliDeleteLikes
  = CliDeleteLikes
  | CliNoDeleteLikes
  deriving (Show, Eq)

mkOauth :: ApiKey 'Consumer 'Private -> ApiKey 'Consumer 'Public -> OAuth
mkOauth (ApiKey priv) (ApiKey pub) = twitterOAuth {oauthConsumerKey = pub, oauthConsumerSecret = priv}

mkAccess :: ApiKey 'Access 'Private -> ApiKey 'Access 'Public -> Credential
mkAccess (ApiKey priv) (ApiKey pub) = Credential [("oauth_token", pub), ("oauth_token_secret", priv)]

mkInfo :: CliOptions a -> TWInfo -> TWInfo
mkInfo (CliOptions priv pub CliAuthMode) = setCredential (mkOauth priv pub) (Credential [])
mkInfo (CliOptions priv pub (CliDeleteMode (CliDeleteOptions a_priv a_pub _ _ _))) =
  setCredential (mkOauth priv pub) (mkAccess a_priv a_pub)
