module TweetUtils.Options
  ( ApiKey (..)
  , ApiKeyType (..)
  , ApiKeyVis (..)
  , AppOptions (..)
  , AppMode (..)
  , DumpTweetOpts (..)
  , mkInfo
  , LogLevel (..)
  , FormatType (..)
  ) where

--------------------------------------------------------------------------------

import Control.Applicative (Alternative (..), liftA2)
import GHC.Generics qualified as GHC
import Control.Monad.IO.Class (MonadIO (..))
import System.Environment (lookupEnv)

--------------------------------------------------------------------------------

import Data.ByteString hiding (empty)
import Data.ByteString qualified as B
import Options.Generic
  ( ParseRecord (..)
  , ParseField (..)
  , ParseFields (..)
  , getOnly
  --, parseRecordWithModifiers
  --, lispCaseModifiers
  )
import Options.Applicative (hsubparser, command, info, progDesc)

--------------------------------------------------------------------------------

import Web.Twitter.Conduit

--------------------------------------------------------------------------------

import TweetUtils.Commands.DumpTweets (FormatType (..))
import TweetUtils.MonadApp (LogLevel (..))

--------------------------------------------------------------------------------

data ApiKeyType = Consumer | Access
data ApiKeyVis  = Public   | Private

data ApiKey (ty :: ApiKeyType) (v :: ApiKeyVis)
  = ApiKey ByteString
  | ApiKeyFile FilePath
  | ApiKeyEnv
  deriving stock (GHC.Generic)
  deriving anyclass (ParseFields, ParseRecord)

instance ParseField (ApiKey ty v) where
  parseField _ label short d =
    (ApiKey <$> parseField @ByteString Nothing label short d)
      <|> (ApiKeyFile <$> parseField @FilePath Nothing fileLabel Nothing Nothing)
      <|> (pure ApiKeyEnv)
    where
      fileLabel = fmap (<> "-file") label

  readField = undefined

  metavar _ = "key"



data AppOptions = AppOptions
  { consumerPrivate ::  Maybe (ApiKey 'Consumer 'Private)
  , consumerPublic  :: Maybe (ApiKey 'Consumer 'Public)
  , logLevel :: LogLevel
  , mode     :: AppMode
  }
  deriving stock (GHC.Generic)
  deriving anyclass ParseRecord

data AppMode
  = Auth
  | DumpTweets DumpTweetOpts
  deriving stock GHC.Generic

instance ParseRecord AppMode where
  parseRecord = fmap getOnly parseRecord

instance ParseFields AppMode where
  parseFields _ _ _ _ =
    hsubparser
      ( command "auth" (info (pure Auth) $ progDesc "get OAuth Keys")
      <> command "dump-tweets" (info parseRecord $ progDesc "dump tweets")
      )


data DumpTweetOpts = DumpTweetOpts
  { accessPrivate :: Maybe (ApiKey 'Access 'Private)
  , accessPublic  :: Maybe (ApiKey 'Access 'Public)
  , outDir           :: FilePath
  , format           :: FormatType
  , downloadMedia    :: Bool
  }
  deriving stock (GHC.Generic)
  deriving anyclass (ParseRecord)


apiKeyBytes :: (MonadIO m) => String -> Maybe (ApiKey ty vis) -> m (Either String ByteString)
apiKeyBytes _ (Just (ApiKey b)) = pure $ pure b
apiKeyBytes _ (Just (ApiKeyFile path)) = fmap pure $ liftIO $ B.readFile path
apiKeyBytes var _ = liftIO $ (lookupEnv var >>= (traverse B.readFile . maybe (Left var) pure))

mkOauth :: ByteString -> ByteString -> OAuth
mkOauth priv pub = twitterOAuth {oauthConsumerKey = pub, oauthConsumerSecret = priv}

mkAccess :: ByteString -> ByteString -> Credential
mkAccess priv pub = Credential [("oauth_token", pub), ("oauth_token_secret", priv)]

authInfo :: TWInfo -> OAuth -> TWInfo
authInfo inf auth = setCredential auth (Credential []) inf

accessInfo :: TWInfo -> OAuth -> Credential -> TWInfo
accessInfo inf auth access = setCredential auth access inf


mkInfo :: (MonadIO m) => AppOptions -> TWInfo ->  m (Either String TWInfo)
mkInfo (AppOptions priv pub _ mode) inf = do
  priv' <- apiKeyBytes "CONSUMER_PRIVATE" priv
  pub' <- apiKeyBytes "CONSUMER_PUBLIC" pub
  auth <- pure $ liftA2 mkOauth priv' pub'
  case mode of
    Auth -> pure $ fmap (authInfo inf) auth
    (DumpTweets (DumpTweetOpts {accessPrivate = aPriv, accessPublic = aPub})) -> do
      aPriv' <- apiKeyBytes "ACCESS_PRIVATE" aPriv
      aPub' <- apiKeyBytes "ACCESS_PUBLIC" aPub
      pure $ liftA2 (accessInfo inf) auth (liftA2 mkAccess aPriv' aPub')
