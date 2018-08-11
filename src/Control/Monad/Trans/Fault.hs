{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | Track your potential faults at the type level.
--
-- In production, the tagging is elided via 'FaultlessT'
--
-- During testing, you can manually set each fault to
-- fail or succeed via 'FaultyT' & 'FaultController'
--
-- This is an extensible way to trigger arbitrary failures
-- within your program. Useful for testing fault tolerance
-- (e.g. retry logic)
module Control.Monad.Trans.Fault
  ( MonadFault (..)
  , faulty
  , FaultlessT (..)
  , FaultyT ()
  , runFaultyT
  , FaultController ()
  , FaultConfig (..)
  , NewFault (..)
  , HasFault (..)
  , showFaultController
  ) where

import           Control.Exception           (SomeException, throwIO)
import           Control.Monad.Base          (MonadBase (..))
import           Control.Monad.IO.Class      (MonadIO (..))
import           Control.Monad.Reader        (MonadReader (..), ReaderT (..))
import           Control.Monad.Trans         (MonadTrans (..))
import           Control.Monad.Trans.Control (ComposeSt, MonadBaseControl (..),
                                              MonadTransControl (..),
                                              defaultLiftBaseWith,
                                              defaultLiftWith, defaultRestoreM,
                                              defaultRestoreT)
import           Data.IORef
import           GHC.TypeLits

-- | Cause a fault named @fault@.
class Monad m => MonadFault (fault :: Symbol) m where
  -- | You're gonna need TypeApplications to get this to work:
  --
  -- >>> fault @"redis"
  fault :: m ()

-- | Automatic instances for MonadTrans
instance {-# OVERLAPPABLE #-} (Monad (t m), MonadTrans t, MonadFault fault m) => MonadFault fault (t m) where
  fault = lift (fault @fault)

-- | Tag an action as a potential fault named @fault@
--
-- @
-- do
--   redisResult <- faulty "redis" $ queryRedis
--   s3Result <- faulty "s3" $ queryS3
--   doStuff redisResult s3Result
-- @
faulty :: forall fault m a. MonadFault fault m => m a -> m a
faulty = (fault @fault *>)

-- | Never fault. Equivalent to 'IdentityT'
newtype FaultlessT m a = FaultlessT { runFaultlessT  :: m a }
  deriving ( Functor, Applicative, Monad, MonadIO
           )

instance Monad m => MonadFault fault (FaultlessT m) where
  fault = pure ()

-- | If the exception is 'Just', we fault. Otherwise, we don't
data FaultConfig = FaultConfig (Maybe SomeException) deriving (Show)

-- | Extensible record of FaultConfigs, each tagged with a fault name
-- at the type level
data FaultController (faults :: [Symbol]) where
  FCNil :: FaultController '[]
  FCCons :: forall f rest. !(IORef FaultConfig) -> FaultController rest -> FaultController (f ': rest)

-- | Create a default, non-faulting 'FaultController'
class NewFault faults where
  newFaultController :: IO (FaultController faults)

instance NewFault '[] where
  newFaultController = pure FCNil

instance NewFault rest => NewFault (f ': rest) where
  newFaultController = do
    ioref <- newIORef (FaultConfig Nothing)
    rest <- newFaultController @rest
    pure $ FCCons ioref rest

-- | Query & modify a 'FaultController' at certain faults
class HasFault (f :: Symbol) faults where
  getFault :: FaultController faults -> IO FaultConfig
  setFault :: FaultConfig -> FaultController faults -> IO ()

instance HasFault goal (goal ': rest) where
  getFault (FCCons ioref _) = readIORef ioref
  setFault new (FCCons ioref _) = atomicWriteIORef ioref new

instance {-# OVERLAPPABLE #-} HasFault goal rest => HasFault goal (f ': rest) where
  getFault (FCCons _ rest) = getFault @goal rest
  setFault new (FCCons _ rest) = setFault @goal new rest

-- | Monad transformer with controller over the specified @faults@
newtype FaultyT (faults :: [Symbol]) m a = FaultyT  { unFaultyT :: ReaderT (FaultController faults) m a }
  deriving ( Functor, Applicative, Monad, MonadIO
           , MonadReader (FaultController faults))

runFaultyT :: FaultController faults -> FaultyT faults m a -> m a
runFaultyT controller = flip runReaderT controller . unFaultyT

-- | If the FaultConfig in teh FaultController for the given @f@ is set, fault. Otherwise,
-- continue as normal
instance forall f faults m. (MonadIO m, HasFault f faults) => MonadFault f (FaultyT faults m) where
  fault = do
    fc <- ask
    FaultConfig mException <- liftIO $ getFault @f fc
    maybe (pure ()) (liftIO . throwIO) mException

-- | For debugging
showFaultController :: FaultController faults -> IO String
showFaultController = \case
  FCNil -> pure "FCNil"
  FCCons ioref rest -> do
    fc <- readIORef ioref
    restStr <- showFaultController rest
    pure $ "(FCNil " ++ show fc ++ " " ++ restStr ++ ")"

-- TODO: MonadBaseControl, MonadBase, other mtl stuff

-- | Instances
-- | FaultlessT

instance MonadBaseControl b m => MonadBaseControl b (FaultlessT m) where
  type StM (FaultlessT m) a = ComposeSt FaultlessT m a
  liftBaseWith = defaultLiftBaseWith
  restoreM     = defaultRestoreM

instance MonadTransControl FaultlessT where
  type StT FaultlessT a = a
  liftWith f = FaultlessT $ f $ runFaultlessT
  restoreT = FaultlessT

instance MonadBase b m => MonadBase b (FaultlessT m) where
  liftBase = FaultlessT . liftBase

instance MonadTrans FaultlessT where
  lift = FaultlessT

-- | FaultyT
instance MonadBaseControl b m => MonadBaseControl b (FaultyT faults m) where
  type StM (FaultyT faults m) a = ComposeSt FaultlessT m a
  liftBaseWith = defaultLiftBaseWith
  restoreM     = defaultRestoreM

instance MonadTransControl (FaultyT faults) where
  type StT (FaultyT faults) a = StT (ReaderT (FaultController faults)) a
  liftWith = defaultLiftWith FaultyT unFaultyT
  restoreT = defaultRestoreT FaultyT

instance MonadBase b m => MonadBase b (FaultyT faults m) where
  liftBase = FaultyT . liftBase

instance MonadTrans (FaultyT faults) where
  lift = FaultyT . lift
