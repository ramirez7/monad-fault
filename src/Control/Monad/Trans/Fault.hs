{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | @monad-fault@ provides an extensible way to trigger arbitrary
-- failures within your programs. Potential faults are tracked in the
-- type system via 'MonadFault' constraints. Useful for testing fault tolerance
-- (e.g. retry logic)
--
-- For instance:
--
-- @
-- mightFault :: ('MonadIO' m, 'MonadFaults' '["redis", "s3", "sqs"] m)
--            => Redis.Connection
--            -> AWS.Env
--            -> m ()
-- mightFault redisConn awsEnv = do
--   s3Result <- 'faulty' @"s3" $ readFromS3 awsEnv
--   'faulty' @"redis" $ writeToRedis redisConn s3Result
--   'faulty' @"sqs" $ writeToSQS awsEnv s3Result
-- @
--
-- In production, we'd run this with 'FaultlessT' which will
-- cause the faults to be elided:
--
-- @
-- runFaultlessT (mightFault redisConn awsEnv) :: IO ()
-- @
--
-- But while testing, we can create a 'FaultController'
-- and run with 'FaultyT'. In our tests, we can set different
-- parts of the program to fault using 'setFault' & 'resetFault'.
-- We can then confirm, for instance, that a Redis blip followed
-- by a retry still results in the correct effects being performed
-- on the world:
--
-- @
-- fc <- 'newFaultController' @'["redis", "s3", "sqs"]
-- 'setFault' @"redis" fc (redisException "uh oh!")
-- 'runFaultyT' fc (mightFault redisConn awsEnv) `shouldThrow` anyException
-- 'resetFault' @"redis" fc
-- 'runFaultyT' fc (mightFault redisConn awsEnv)
-- performChecks
-- @
module Control.Monad.Trans.Fault
  (
  -- * Causing faults
    fault
  , faulty
  -- * The MonadFault Class
  , MonadFault (..)
  , MonadFaults
  -- * The FaultlessT & FaultyT monad transformers
  , FaultlessT (..)
  , runFaultlessT
  , FaultyT (..)
  , runFaultyT
  -- * Controlling faults
  , newFaultController
  , setFault
  , resetFault
  -- * Debugging
  , printFaultController
  , showFaultController
  , askFaultController
  -- * Internals
  , FaultController (..)
  , FaultConfig (..)
  , NewFault ()
  , HasFault (..)
  ) where

import           Control.Exception            (Exception, SomeException (..),
                                               throwIO)
import           Control.Monad                ((>=>))
import           Control.Monad.Base           (MonadBase (..))
import           Control.Monad.Catch          (MonadCatch, MonadThrow)
import           Control.Monad.Except         (MonadError)
import           Control.Monad.IO.Class       (MonadIO (..))
import           Control.Monad.Logger         (MonadLogger (..))
import           Control.Monad.Reader         (MonadReader (..), ReaderT (..))
import           Control.Monad.State          (MonadState)
import           Control.Monad.Trans          (MonadTrans (..))
import           Control.Monad.Trans.Control  (ComposeSt, MonadBaseControl (..),
                                               MonadTransControl (..),
                                               defaultLiftBaseWith,
                                               defaultLiftWith, defaultRestoreM,
                                               defaultRestoreT)
import           Control.Monad.Trans.Identity (IdentityT (..))
import           Control.Monad.Trans.Resource (MonadResource (..))
import           Data.IORef
import           Data.Kind                    (Constraint)
import           Data.Proxy
import           Data.Kind                    (Type)
import           Data.Typeable

-- | @m@ is capable of having @fault@. @fault@s are
-- named with type-level strings
class Monad m => MonadFault fault m where
  -- | Cause a fault named @fault@
  faultPrx :: Proxy fault -> m ()

-- | Cause a fault named @fault@
--
-- Meant to be used w/@TypeApplications@
--
-- >>> fault @"redis" :: MonadFault "redis" m => m ()
fault :: forall fault m. MonadFault fault m => m ()
fault = faultPrx (Proxy @fault)

-- | One @m@ frequently has many potential @faults@
type family MonadFaults (faults :: [k]) (m :: Type -> Type) :: Constraint where
  MonadFaults '[] m = ()
  MonadFaults (fault ': rest) m = (MonadFault fault m, MonadFaults rest m)

-- | Automatic instances for MonadTrans
instance {-# OVERLAPPABLE #-} (Monad (t m), MonadTrans t, MonadFault fault m) => MonadFault fault (t m) where
--  faultPrx _ = lift (fault @fault)
  faultPrx _ = lift (faultPrx (Proxy @fault))

-- | Tag an action as a potential fault named @fault@
--
-- >>> faulty @"failure name" $ mightFail
faulty :: forall fault m a. MonadFault fault m => m a -> m a
faulty = ((faultPrx (Proxy @fault)) *>)

-- | Can never fault.
newtype FaultlessT m a = FaultlessT { unFaultlessT  :: IdentityT m a }
  deriving ( Functor, Applicative, Monad, MonadIO
           , MonadLogger, MonadError e, MonadState s
           , MonadReader r, MonadCatch, MonadThrow
           )

-- | Unwrap 'FaultlessT', ignoring all possible faults
runFaultlessT :: FaultlessT m a -> m a
runFaultlessT = runIdentityT . unFaultlessT

instance Monad m => MonadFault fault (FaultlessT m) where
  faultPrx _ = pure ()

-- | If the exception is 'Just', we fault. Otherwise, we don't
data FaultConfig = FaultConfig (Maybe SomeException) deriving (Show)

-- | Extensible record of FaultConfigs, each tagged with a fault name
-- at the type level
data FaultController (faults :: [k]) where
  FCNil :: FaultController '[]
  FCCons :: forall f rest. Typeable f => Proxy f -> !(IORef FaultConfig) -> FaultController rest -> FaultController (f ': rest)

-- | Create a default, non-faulting 'FaultController'
class NewFault faults where
  -- | Create a 'FaultController' initially configured to never fault
  newFaultController :: IO (FaultController faults)

instance NewFault '[] where
  newFaultController = pure FCNil

instance (Typeable f, NewFault rest) => NewFault (f ': rest) where
  newFaultController = do
    ioref <- newIORef (FaultConfig Nothing)
    rest <- newFaultController @rest
    pure $ FCCons Proxy ioref rest

-- | Query & modify a 'FaultController' at certain faults
class HasFault f faults where
  getFaultConfig :: FaultController faults -> IO FaultConfig
  setFaultConfig :: FaultConfig -> FaultController faults -> IO ()

-- | Set a @fault@ to throw a given 'Exception'
setFault :: forall fault faults e
          . (HasFault fault faults, Exception e)
         => e
         -> FaultController faults
         -> IO ()
setFault e fc = setFaultConfig @fault (FaultConfig $ Just $ SomeException e) fc

-- | Set a @fault@ to not fail.
resetFault :: forall fault faults
          . (HasFault fault faults)
         => FaultController faults
         -> IO ()
resetFault fc = setFaultConfig @fault (FaultConfig Nothing) fc

instance HasFault goal (goal ': rest) where
  getFaultConfig (FCCons _ ioref _) = readIORef ioref
  setFaultConfig new (FCCons _ ioref _) = atomicWriteIORef ioref new

instance {-# OVERLAPPABLE #-} HasFault goal rest => HasFault goal (f ': rest) where
  getFaultConfig (FCCons _ _ rest) = getFaultConfig @goal rest
  setFaultConfig new (FCCons _ _ rest) = setFaultConfig @goal new rest

-- | Monad transformer that allows the caller to control which faults occur
newtype FaultyT (faults :: [k]) m a = FaultyT  { unFaultyT :: ReaderT (FaultController faults) m a }
  deriving ( Functor, Applicative, Monad, MonadIO
           , MonadLogger, MonadError e, MonadState s
           , MonadCatch, MonadThrow
           )

-- | Run a 'FaultyT', causing faults along the way according to
-- the given 'FaultController'
runFaultyT :: FaultController faults -> FaultyT faults m a -> m a
runFaultyT controller = flip runReaderT controller . unFaultyT

-- | Access the 'FaultController' within a 'FaultyT'. You
-- usually can't use this because you'll write your programs
-- in terms of @MonadFault "whatever" m =>@
askFaultController :: Monad m => FaultyT faults m (FaultController faults)
askFaultController = FaultyT ask

instance forall f faults m. (MonadIO m, HasFault f faults) => MonadFault f (FaultyT faults m) where
  faultPrx _ = do
    fc <- askFaultController
    FaultConfig mException <- liftIO $ getFaultConfig @f fc
    maybe (pure ()) (liftIO . throwIO) mException

-- | Create the output of 'printFaultController'
showFaultController :: FaultController faults -> IO String
showFaultController = \case
  FCNil -> pure "FCNil"
  FCCons prx ioref rest -> do
    fc <- readIORef ioref
    restStr <- showFaultController rest
    pure $ "(FCCons @" ++ show (typeRep prx) ++ " " ++ show fc ++ " " ++ restStr ++ ")"

-- | >>> fc <- newFaultController @'["x", "y"]
-- >>> printFaultController fc
-- (FCCons @"x" FaultConfig Nothing (FCCons @"y" FaultConfig Nothing FCNil))
printFaultController :: FaultController faults -> IO ()
printFaultController = showFaultController >=> putStrLn

-- FaultlessT Instances
instance MonadBaseControl b m => MonadBaseControl b (FaultlessT m) where
  type StM (FaultlessT m) a = ComposeSt FaultlessT m a
  liftBaseWith = defaultLiftBaseWith
  restoreM     = defaultRestoreM

instance MonadTransControl FaultlessT where
  type StT FaultlessT a = StT IdentityT a
  liftWith = defaultLiftWith FaultlessT unFaultlessT
  restoreT = defaultRestoreT FaultlessT

instance MonadBase b m => MonadBase b (FaultlessT m) where
  liftBase = FaultlessT . liftBase

instance MonadTrans FaultlessT where
  lift = FaultlessT . IdentityT

instance MonadResource m => MonadResource (FaultlessT m) where
  liftResourceT = lift . liftResourceT

-- FaultyT Instances
instance MonadBaseControl b m => MonadBaseControl b (FaultyT faults m) where
  type StM (FaultyT faults m) a = ComposeSt (FaultyT faults) m a
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

-- | Even though 'FaultyT' has a 'ReaderT' within, our 'MonadReader' instance
-- is just a lift
instance MonadReader r m => MonadReader r (FaultyT faults m) where
  ask = lift ask
  local f (FaultyT (ReaderT rf)) = FaultyT $ ReaderT $ \r -> local f (rf r)

instance MonadResource m => MonadResource (FaultyT faults m) where
  liftResourceT = lift . liftResourceT

