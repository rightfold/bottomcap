module Bcapc.Target.EcmaScript.LinkSpec
  ( spec
  ) where

import Data.HashSet (HashSet)
import Test.Hspec (Spec, it, shouldBe)

import qualified Data.HashSet as HashSet

import Bcapc.Target.EcmaScript.Link

import Bcapc.Name (Global (..))
import Bcapc.Target.EcmaScript.Object (EcmaScriptObject (..))

spec :: Spec
spec = do
  it "succeeds on empty inputs" $
    linkEcmaScriptObjects [] HashSet.empty
      `shouldBe` Right ""

  it "succeeds when everything was defined" $
    let
      objects :: [EcmaScriptObject]
      objects = [ EcmaScriptObject (Global [] "main") [Global [] "m4in"] "A"
                , EcmaScriptObject (Global [] "m4in") [Global [] "ma1n"] "B"
                , EcmaScriptObject (Global [] "ma1n") []                 "C" ]

      exports :: HashSet Global
      exports = HashSet.singleton (Global [] "main")
    in
      linkEcmaScriptObjects objects exports
        `shouldBe` Right "ABC"

  it "fails when dependencies were not defined" $
    let
      objects :: [EcmaScriptObject]
      objects = [EcmaScriptObject (Global [] "main") [Global [] "m4in"] "A"]
    in
      linkEcmaScriptObjects objects HashSet.empty
        `shouldBe` Left Undefined

  it "fails when a value was redefined" $
    let
      objects :: [EcmaScriptObject]
      objects = [ EcmaScriptObject (Global [] "main") [] "A"
                , EcmaScriptObject (Global [] "main") [] "B" ]
    in
      linkEcmaScriptObjects objects HashSet.empty
        `shouldBe` Left (Redefinition (Global [] "main"))

  it "fails when exports were not defined" $
    let
      exports :: HashSet Global
      exports = HashSet.singleton (Global [] "main")
    in
      linkEcmaScriptObjects [] exports
        `shouldBe` Left Undefined
