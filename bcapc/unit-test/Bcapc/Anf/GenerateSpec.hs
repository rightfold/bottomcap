module Bcapc.Anf.GenerateSpec
  ( spec
  ) where

import Test.Hspec (Spec, it, shouldBe)

import Bcapc.Anf.Generate
import Bcapc.Anf.TestData

import Bcapc.Anf (Anf (..), AnfExpression (..), AnfValue (..))

spec :: Spec
spec = do
  it "generates the correct administrative normal form for an example" $
    let
      example :: GenerateAnf AnfValue
      example = do
        g <- anfGlobal glo1
        t <- anfConstant true
        f <- anfLambda $ \x -> anfApply g x
        r <- anfApply f t
        pure r

      expected :: Anf
      expected =
        Anf [ (syn3, AnfLambda syn1 (Anf [ (syn2, AnfApply aglo1 asyn1) ] asyn2))
            , (syn4, AnfApply asyn3 atrue) ]
            asyn4
    in
      execGenerateAnf example `shouldBe` expected
