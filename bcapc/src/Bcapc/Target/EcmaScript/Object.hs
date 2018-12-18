{-# LANGUAGE StrictData #-}

-- |
-- Object files in the ECMAScript target.
module Bcapc.Target.EcmaScript.Object
  ( EcmaScriptObject (..)
  ) where

import Control.Lens ((<&>))
import Data.ByteString (ByteString)
import Data.HashSet (HashSet)
import Data.Serialize (Serialize, get, put)

import qualified Data.HashSet as HashSet

import Bcapc.Name (Global)

-- |
-- An object defines a top-level value. It contains ECMAScript code that
-- defines it, and information needed for linking with other objects.
data EcmaScriptObject =
  EcmaScriptObject
    { -- |
      -- The name of the value that this object defines.
      esObjectName :: Global

      -- |
      -- The names of the values that this object references.
    , esObjectDependencies :: HashSet Global

      -- |
      -- The ECMAScript code that defines the value.
    , esObjectCode :: ByteString }

instance Serialize EcmaScriptObject where
  put (EcmaScriptObject a b c) = put (a, HashSet.toList b, c)
  get = get <&> \(a, b, c) -> EcmaScriptObject a (HashSet.fromList b) c
