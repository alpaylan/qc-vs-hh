{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-full-laziness #-}
{-# OPTIONS_GHC -fno-cse #-}

module Etna.Lib.Strategy.Falsify
  ( fsDefaults,
    fsRunGen,
  )
where

import Control.Monad (unless)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Etna.Lib.Types
import Etna.Lib.Util (maxCap)
import System.IO.Unsafe (unsafePerformIO)
import Test.Falsify.Generator (Gen)
import Test.Falsify.Property
import Test.Tasty.Falsify
import Test.Tasty.Runners hiding (Result)
import qualified Test.Tasty.Providers as TP

fsDefaults :: Int
fsDefaults = maxCap

updateCounter :: IORef Int -> ()
updateCounter ref = unsafePerformIO (modifyIORef' ref (+ 1))
{-# NOINLINE updateCounter #-}

record :: (Show a) => IORef String -> a -> ()
record ref a = unsafePerformIO (writeIORef ref (show a))
{-# NOINLINE record #-}

fsRunGen :: (Show a) => Int -> Approach -> Gen a -> Strategy a
fsRunGen cap app g task = do
  testsRef <- newIORef 0
  discardsRef <- newIORef 0
  counterexampleRef <- newIORef ""

  let shouldDiscard pre =
        case app of
          Naive -> not pre
          Correct -> False

      prop = do
        a <- gen g
        record counterexampleRef a `seq` pure ()
        let (pre, post) = task a
        if shouldDiscard pre
          then do
            updateCounter discardsRef `seq` discard
          else do
            updateCounter testsRef `seq` pure ()
            unless post $ testFailed (show a)

      testOptions = TestOptions
          { expectFailure = DontExpectFailure,
            overrideVerbose = Nothing,
            overrideMaxShrinks = Just 0,
            overrideNumTests = Just (fromIntegral cap),
            overrideMaxRatio = Just (fromIntegral cap)
          }
      go (SingleTest _ t) = TP.run mempty t (\_ -> pure ())

  tastyResult <- go $ testPropertyWith testOptions "falsify" prop

  let ok = resultSuccessful tastyResult
      status = if ok then "Finished" else "Failed"

  tests <- readIORef testsRef
  discardsCount <- readIORef discardsRef
  counterexample <- if ok then (pure "") else (readIORef counterexampleRef)

  return Result { discards = Just discardsCount, ..}
