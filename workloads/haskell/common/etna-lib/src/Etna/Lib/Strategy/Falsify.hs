{-# LANGUAGE RecordWildCards #-}

module Etna.Lib.Strategy.Falsify
  ( fsDefaults,
    fsRunGen,
  )
where

import Control.Monad (unless)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Maybe
import Etna.Lib.Types
import Etna.Lib.Util (maxCap)
import System.IO.Unsafe (unsafePerformIO)
import Test.Falsify.Generator (Gen)
import qualified Test.Falsify.Interactive as FS
import Test.Falsify.Property

fsDefaults :: Int
fsDefaults = maxCap

fsRunGen :: (Show a) => Int -> Approach -> Gen a -> Strategy a
fsRunGen cap app g task = do
  discardsRef <- newIORef 0

  let shouldDiscard :: Bool -> Bool
      shouldDiscard pre =
        case app of
          Naive -> not pre
          Correct -> False

      run = do
        a <- gen g
        let (pre, post) = task a
        if shouldDiscard pre
          then
            unsafePerformIO (modifyIORef' discardsRef (+ 1)) `seq` discard
          else
            unless post $
              testFailed (show a)

      go n
        | n >= cap = pure (Nothing, n)
        | otherwise = do
            result <- FS.falsify run
            case result of
              Nothing -> go (n + 1)
              Just ce -> pure (Just ce, n + 1)

  (maybeFailure, tests) <- go 0
  discardsCount <- readIORef discardsRef

  let status = case maybeFailure of
        Nothing -> "Finished"
        Just _ -> "Failed"

  pure
    Result
      { counterexample = fromMaybe "" maybeFailure,
        discards = Just discardsCount,
        ..
      }
