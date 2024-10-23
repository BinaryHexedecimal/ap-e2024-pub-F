module SPC_Tests (tests) where

import Control.Concurrent (threadDelay)
import Data.IORef
import SPC
import Test.Tasty (TestTree, localOption, mkTimeout, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.Runners (Status(Done))

tests :: TestTree
tests =
  localOption (mkTimeout 3000000) $
    testGroup
      "SPC"
      [{- testCase "ping" $ do
          spc <- startSPC
          x <- pingSPC spc
          x @?= 4 -}
      -- testCase "ping_ping" $ do
      --     spc <- startSPC

      --     x <- pingSPC spc
      --     x @?= 0

      --     y <- pingSPC spc
      --     y @?= 1

      --     z <- pingSPC spc
      --     z @?= 2   
       testCase "adding job" $ do
          spc <- startSPC
          _ <- jobAdd spc $ Job (pure ()) 1
          pure ()   
      --  ,testCase "querying job" $ do
      --     spc <- startSPC
      --     j <- jobAdd spc $ Job (pure ()) 1
      --     r <- jobStatus spc j
      --     r @?= Just JobPending  
      --  ,testCase "canceling job" $ do
      --     spc <- startSPC
      --     j <- jobAdd spc $ Job (pure ()) 1
      --     jobCancel spc j
      --     r <- jobStatus spc j
      --     r @?= Just (JobDone DoneCancelled)
      ,testCase "running job" $ do
          ref <- newIORef False
          spc <- startSPC
          j <- jobAdd spc $ Job (writeIORef ref True) 1
          r <- jobWait spc j
          r @?= Just Done_
          x <- readIORef ref
          x @?= True

      ,testCase "crash" $ do
          spc <- startSPC
          j1 <- jobAdd spc $ Job (error "boom") 1
          r1 <- jobWait spc j1
          r1 @?= Just DoneCrashed
          -- Ensure new jobs can still work.
          ref1 <- newIORef False
          j2 <- jobAdd spc $ Job (writeIORef ref1 True) 1
          r2 <- jobWait spc j2
          r2 @?= Just Done_
          v <- readIORef ref1
          v @?= True

       ,testCase "timeout" $ do
          spc <- startSPC
          ref <- newIORef False
          j <- jobAdd spc $ Job (threadDelay 2000000 >> writeIORef ref True) 1
          r1 <- jobStatus spc j
          r1 @?= Just JobRunning
          r2 <- jobWait spc j
          r2 @?= Just DoneTimeout

          ]
