module TweetUtils.MonadApp where ()

--------------------------------------------------------------------------------

import Control.Monad.IO.Class (MonadIO (..))

--------------------------------------------------------------------------------

data Named = Symbol := Type


type family :: AllConfig (row :: [Named]) (m :: Type -> Type) :: Constraint where
  AllConfig '[] = ()
  AllConfig (r ': row) = (MonadConfig r m, MonadConfig row m)

 --------------------------------------------------------------------------------

type AppConfig =
  '[ "twInfo" ':= TWInfo
   , "manager" ':= Manager
   ]

class
  ( MonadIO m
  , AllConfig cfg m
  , AllConfig AppConfig m
  ) =>
  MonadApp (m :: Type -> Type) (cfg :: [Named])

class
  (Monad m) =>
  MonadConfig (name :: Symbol) (r :: Type) (m :: Type -> Type) where
  config :: m r

--------------------------------------------------------------------------------

instance
  ( HasField name r a
  , MonadReader r m
  ) =>
  MonadConfig name a m where
  config = asks $ getField @name

--------------------------------------------------------------------------------

newtype AppT r m a =
  AppT { runAppT :: ReaderT r m a }
