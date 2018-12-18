module Bcapc.Name.MangleSpec
  ( spec
  ) where

import Data.ByteString.Builder (Builder, toLazyByteString)
import Test.Hspec (Expectation, Spec, describe, it, shouldBe)

import Bcapc.Name.Mangle

import Bcapc.Name (Local (..))

spec :: Spec
spec =
  describe "mangleLocal" $ do
    it "mangles user-given locals" $
      let
        example :: Local
        example = Local "amazingFuncti0n++"

        expected :: Builder
        expected = "bcapl38zamazzingzufuncti0nzs0000002bzs0000002b"
      in
        mangleLocal example `shouldBeB` expected

    it "mangles synthesized locals" $
      let
        example :: Local
        example = Synthesized 42

        expected :: Builder
        expected = "bcaps2z42"
      in
        mangleLocal example `shouldBeB` expected

shouldBeB :: Builder -> Builder -> Expectation
shouldBeB a b = toLazyByteString a `shouldBe` toLazyByteString b
