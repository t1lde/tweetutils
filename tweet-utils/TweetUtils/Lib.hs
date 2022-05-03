module TweetUtils.Lib (runWithOptions) where


--------------------------------------------------------------------------------

import Control.Arrow ((>>>))
import GHC.Records as GHC (getField, HasField (..))
import System.Exit (die)

--------------------------------------------------------------------------------

import Control.Monad.Reader
  (ReaderT (..), asks)

--------------------------------------------------------------------------------

import Web.Twitter.Conduit

--------------------------------------------------------------------------------

import TweetUtils.Options (AppOptions (..), AppMode (..), DumpTweetOpts (..), mkInfo)
import TweetUtils.Commands.GetAuth (apiAuth)
import TweetUtils.Commands.DumpTweets (apiDumpTweets, type DumpTweetInfo)
import TweetUtils.MonadApp
  (MonadConfig (..), MonadApp, LogLevel (..), callTwitter)

--------------------------------------------------------------------------------

runWithOptions :: AppOptions -> IO ()
runWithOptions opts = do
  apiManager <- newManager tlsManagerSettings
  inf <-
    mkInfo opts proxySettings
      >>= either handleErr pure

  appMode inf apiManager opts
  return ()
  where
    handleErr x = die $ x <> " not found in env & flag not provided"

data AppInfo a =
  AppInfo
    { twInfo :: TWInfo
    , manager :: Manager
    , logLevel :: LogLevel
    , opts :: a
    }

instance {-# OVERLAPPING #-}
  MonadConfig "twInfo" TWInfo (ReaderT (AppInfo opts) IO) where
  config = asks (getField @"twInfo")

instance {-# OVERLAPPING #-}
  MonadConfig "manager" Manager (ReaderT (AppInfo opts) IO) where
  config = asks (getField @"manager")

instance  {-# OVERLAPPING #-}
  MonadConfig "logLevel" LogLevel (ReaderT (AppInfo opts) IO) where
  config = asks (getField @"logLevel")

instance {-# OVERLAPPABLE #-}
  (GHC.HasField name opts a) =>
  MonadConfig name a (ReaderT (AppInfo opts) IO) where
  config = asks (getField @"opts" >>> getField @name)

instance MonadApp DumpTweetInfo (ReaderT (AppInfo DumpTweetOpts) IO) where

appMode :: TWInfo -> Manager -> AppOptions -> IO ()
appMode (TWInfo (TWToken oauth _) _) mgr (AppOptions { mode = Auth }) = apiAuth oauth mgr
appMode info mgr (AppOptions { mode = (DumpTweets opts), logLevel = lvl }) =
  (callTwitter accountVerifyCredentials >>= apiDumpTweets)
    `runReaderT`
      AppInfo
        { twInfo = info
        , manager = mgr
        , logLevel = lvl
        , opts = opts
        }


proxySettings :: TWInfo
proxySettings = def