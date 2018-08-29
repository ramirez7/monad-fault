{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}

module MonadFaultSpec (spec) where

import           Test.Hspec

import           Control.Exception         (Exception (..), SomeException)
import           Control.Monad.IO.Class    (MonadIO (..))
import           Data.IORef

import           Control.Monad.Trans.Fault

spec :: Spec
spec = do
  describe "FaultlessT" $ do
    it "should never fail" $ do
      ioref1 <- newIORef False
      ioref2 <- newIORef False
      (runFaultlessT $ testProgram ioref1 ioref2) `shouldReturn` ()
      readIORef ioref1 `shouldReturn` True
      readIORef ioref2 `shouldReturn` True

  describe "FaultyT" $ do
    it "should fail when & where told to" $ do
      -- Initialization
      fc <- newFaultController @[Fault1, Fault2]
      ioref1 <- newIORef False
      ioref2 <- newIORef False

      let reset = do
            resetFault @Fault1 fc
            resetFault @Fault2 fc
            atomicWriteIORef ioref1 False
            atomicWriteIORef ioref2 False

      let runTest = runFaultyT fc $ testProgram ioref1 ioref2

      -- No failures
      runTest `shouldReturn` ()
      readIORef ioref1 `shouldReturn` True
      readIORef ioref2 `shouldReturn` True

      reset

      -- Fail at Fault1
      setFault @Fault1 TestException1 fc
      runTest `shouldThrow` testException1
      readIORef ioref1 `shouldReturn` False
      readIORef ioref2 `shouldReturn` False

      reset

      -- Fail at Fault2
      setFault @Fault2 TestException2 fc
      runTest `shouldThrow` testException2
      readIORef ioref1 `shouldReturn` True
      readIORef ioref2 `shouldReturn` False

      reset

data TestFaults = Fault1 | Fault2

testProgram
  :: (MonadFaults '[Fault1, Fault2] m, MonadIO m)
  => IORef Bool
  -> IORef Bool
  -> m ()
testProgram ioref1 ioref2 = do
  faulty @'Fault1 $ liftIO $ atomicWriteIORef ioref1 True
  faulty @'Fault2 $ liftIO $ atomicWriteIORef ioref2 True

data TestException = TestException1 | TestException2 deriving (Show, Eq)
instance Exception TestException
testException1 :: Selector SomeException
testException2 :: Selector SomeException
testException1 = testException TestException1
testException2 = testException TestException2
testException :: TestException -> Selector SomeException
testException testE someE = case fromException someE of
  Just e | e == testE -> True
  _      -> False
