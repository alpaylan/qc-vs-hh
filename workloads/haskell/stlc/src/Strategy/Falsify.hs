{-# LANGUAGE TemplateHaskell #-}

module Strategy.Falsify where

import Data.List.NonEmpty (NonEmpty (..))
import Etna.Lib
import Impl
import Spec
import qualified Test.Falsify.Generator as Gen
import Test.Falsify.Generator (Gen)
import qualified Test.Falsify.Range as Range

class FGen a where
  fgen :: Gen a

instance FGen Typ where
  fgen = do
    height <- Gen.int (Range.between (0, 1000))
    genTyp height
    where
      genTyp :: Int -> Gen Typ
      genTyp height
        | height <= 0 = pure TBool
        | otherwise = do
            left <- Gen.int (Range.between (0, height - 1))
            right <- Gen.int (Range.between (0, height - 1))
            Gen.frequency
              [ (1, pure TBool),
                (3, TFun <$> genTyp left <*> genTyp right)
              ]

instance FGen Expr where
  fgen = do
    height <- Gen.int (Range.between (0, 1000))
    genExpr height
    where
      genExpr :: Int -> Gen Expr
      genExpr height
        | height <= 0 =
            Gen.frequency
              [ (1, Var <$> Gen.int (Range.withOrigin (-1000, 1000) 0)),
                (1, Bool <$> Gen.elem (True :| [False]))
              ]
        | otherwise = do
            typHeight <- Gen.int (Range.between (0, height - 1))
            bodyHeight <- Gen.int (Range.between (0, height - 1))
            left <- Gen.int (Range.between (0, height - 1))
            right <- Gen.int (Range.between (0, height - 1))
            Gen.frequency
              [ (1, Var <$> Gen.int (Range.withOrigin (-1000, 1000) 0)),
                (1, Bool <$> Gen.elem (True :| [False])),
                (3, Abs <$> genTyp typHeight <*> genExpr bodyHeight),
                (3, App <$> genExpr left <*> genExpr right)
              ]

      genTyp :: Int -> Gen Typ
      genTyp height
        | height <= 0 = pure TBool
        | otherwise = do
            left <- Gen.int (Range.between (0, height - 1))
            right <- Gen.int (Range.between (0, height - 1))
            Gen.frequency
              [ (1, pure TBool),
                (3, TFun <$> genTyp left <*> genTyp right)
              ]

$( mkStrategies
     [|fsRunGen fsDefaults Naive fgen|]
     [ 'prop_SinglePreserve,
       'prop_MultiPreserve
     ]
 )
