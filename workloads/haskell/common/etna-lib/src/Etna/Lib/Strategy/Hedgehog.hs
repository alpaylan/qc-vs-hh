{-# LANGUAGE RecordWildCards #-}

module Etna.Lib.Strategy.Hedgehog
  ( hhDefaults,
    hhRunGen,
  )
where

import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Etna.Lib.Types
import Etna.Lib.Util (maxCap)
import qualified Hedgehog as HH

hhDefaults :: Int
hhDefaults = maxCap

hhRunGen :: (Show a) => Int -> Approach -> HH.Gen a -> Strategy a
hhRunGen cap app gen task = do
  testsRef <- newIORef 0
  discardsRef <- newIORef 0
  counterexampleRef <- newIORef ""

  let prop =
        HH.withTests (fromIntegral cap) $
          HH.withShrinks 0 $
            HH.property $ do
              a <- HH.forAll gen
              HH.evalIO $ writeIORef counterexampleRef (show a)
              let (pre, post) = task a
              case app of
                Naive ->
                  if pre
                    then do
                      HH.evalIO $ modifyIORef' testsRef (+ 1)
                      HH.assert post
                    else do
                      HH.evalIO $ modifyIORef' discardsRef (+ 1)
                      HH.discard
                Correct -> do
                  HH.evalIO $ modifyIORef' testsRef (+ 1)
                  HH.assert post

  ok <- HH.check prop
  tests <- readIORef testsRef
  discards <- Just <$> readIORef discardsRef
  counterexample <- readIORef counterexampleRef
  let status = if ok then "Finished" else "Failed"
  if ok
    then return Result {counterexample = "", ..}
    else return Result {..}
