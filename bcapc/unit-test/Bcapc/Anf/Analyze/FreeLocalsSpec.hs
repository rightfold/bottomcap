module Bcapc.Anf.Analyze.FreeLocalsSpec
  ( spec
  ) where

import Data.HashSet (HashSet)
import Test.Hspec (Spec, it, shouldBe)

import Bcapc.Anf.Analyze.FreeLocals
import Bcapc.Anf.TestData

import Bcapc.Anf (Anf (..), AnfExpression (..))
import Bcapc.Name (Local)

spec :: Spec
spec =
  it "returns all and only free locals" $
    let
      example :: Anf
      example =
        Anf [ (syn3, AnfLambda syn1 (Anf [ (syn2, AnfApply aglo1 asyn1) ] asyn2))
            , (syn4, AnfApply asyn4 atrue) ]
            asyn5

      expected :: HashSet Local
      expected = [syn4, syn5]
    in
      anfFreeLocals example `shouldBe` expected
