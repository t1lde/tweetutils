module TweetUtils.MonadApp
  ( MonadConfig (..)
  , MonadAppBase
  , MonadApp
  , Named ((:=))
  , LogLevel (..)
  , liftAppConf
  , logNormal
  , logDebug
  , callTwitter
  , streamTwitter
  ) where

--------------------------------------------------------------------------------

import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (FromJSON)
import Control.Monad (when)
import Data.Kind (Type, Constraint)
import GHC.TypeLits (Symbol)
import Control.Applicative (liftA2)
import GHC.Generics qualified as GHC

--------------------------------------------------------------------------------

import Data.Text qualified as T
import Data.Text.IO qualified as T
import Options.Generic
  ( ParseRecord (..)
  , ParseField (..)
  , ParseFields (..)
  )

--------------------------------------------------------------------------------

import Web.Twitter.Conduit
  (HasParam, Manager, TWInfo, APIRequest, call, sourceWithMaxId)
import Conduit ((.|), sinkList, runConduit)
import Web.Twitter.Conduit.Base (ResponseBodyType (..))
import Web.Twitter.Types.Lens (AsStatus)

--------------------------------------------------------------------------------

data Named = Symbol := Type


type family AllConfig (row :: [Named]) (m :: Type -> Type) :: Constraint where
  AllConfig '[] m = ()
  AllConfig ((name ':= r) ': row) m = (MonadConfig name r m, AllConfig row m)

 --------------------------------------------------------------------------------

type AppConfig =
  '[ "twInfo" ':= TWInfo
   , "manager" ':= Manager
   , "logLevel" ':= LogLevel
   ]

data LogLevel = Quiet | Normal | Debug
  deriving stock (Show, Eq, Ord, GHC.Generic, Read)
  deriving anyclass (ParseFields, ParseField, ParseRecord)


--------------------------------------------------------------------------------

type MonadAppBase m = (AllConfig AppConfig m, MonadIO m)

class
  ( MonadAppBase m
  , AllConfig cfg m
  ) =>
  MonadApp (cfg :: [Named]) (m :: Type -> Type)

class
  (Monad m) =>
  MonadConfig (name :: Symbol) (r :: Type) (m :: Type -> Type) where
  config :: m r

liftAppConf :: (AllConfig AppConfig m) => (TWInfo -> Manager -> r) -> m r
liftAppConf f = liftA2 f (config @"twInfo") (config @"manager")

whenLogLevel :: (MonadAppBase m) => LogLevel -> m () -> m ()
whenLogLevel lvl eff = config @"logLevel" >>= (`when` eff) . (>= lvl)

logNormal :: (MonadAppBase m) => T.Text -> m ()
logNormal = whenLogLevel Normal . liftIO . T.putStrLn

logDebug :: (MonadAppBase m) => T.Text -> m ()
logDebug = whenLogLevel Debug . liftIO . T.putStrLn

callTwitter ::
  ( MonadAppBase m
  , ResponseBodyType response
  ) =>
  APIRequest api response -> m response
callTwitter req =  liftAppConf call >>= liftIO . ($ req) -- bruh

streamTwitter ::
  ( MonadAppBase m
  , FromJSON response
  , AsStatus response
  , HasParam "max_id" Integer api
  ) =>
  APIRequest api [response] -> m [response]
streamTwitter req =
  liftAppConf sourceWithMaxId
    >>= liftIO . runConduit . (.| sinkList) . ($ req)
