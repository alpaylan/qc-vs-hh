{-# LANGUAGE TemplateHaskell #-}

module Strategy.Hedgehog where

import Etna.Lib
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Impl
import Spec

class HGen a where
  hgen :: HH.Gen a

instance HGen Typ where
  hgen =
    Gen.recursive
      Gen.choice
      [pure TBool]
      [TFun <$> hgen <*> hgen]

instance HGen Expr where
  hgen =
    Gen.recursive
      Gen.choice
      [ Var <$> Gen.int (Range.linear (-1000) 1000),
        Bool <$> Gen.choice [pure True, pure False]
      ]
      [Abs <$> hgen <*> hgen, App <$> hgen <*> hgen]

$( mkStrategies
     [|hhRunGen hhDefaults Naive hgen|]
     [ 'prop_SinglePreserve,
       'prop_MultiPreserve
     ]
 )
