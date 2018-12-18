{-# LANGUAGE StrictData #-}

-- |
-- bcapc.anf.
module Bcapc.Anf
  ( AnfObject (..)
  , Anf (..)
  , AnfExpression (..)
  , AnfCase (..)
  , AnfValue (..)
  ) where

import Data.Vector (Vector)

import Bcapc.Constant (Constant)
import Bcapc.Intrinsic (Intrinsic)
import Bcapc.Name (Field, Global, Local)

data AnfObject =
  AnfObject
    { anfObjectName :: Global
    , anfObjectCode :: Anf }

-- |
-- A list of bindings paired with a result value. Each binding is in scope of
-- any subsequent binding and the result value, except where shadowed by a
-- subsequent binding.
data Anf =
  Anf
    { anfBindings :: Vector (Local, AnfExpression)
    , anfResult :: AnfValue }

-- |
-- A reducible unit operates only on values.
data AnfExpression
  -- |
  -- Apply a function to a value.
  = AnfApply AnfValue AnfValue

  -- |
  -- Construct a function with a parameter.
  | AnfLambda Local Anf

  -- |
  -- Construct a record value.
  | AnfRecord [(Field, AnfValue)]

  -- |
  -- Project a field from a record value.
  | AnfProject Field AnfValue

  -- |
  -- Inject a value into a variant.
  | AnfInject Field AnfValue

  -- |
  -- Analyze a variant value.
  | AnfMatch AnfValue [AnfCase]

data AnfCase =
  AnfCase
    { anfCaseField :: Field
    , anfCaseValue :: Local
    , anfCaseBody  :: Anf }

-- |
-- A simple value that contains no other values and is cheap to evaluate many
-- times.
data AnfValue
  = AnfGlobal Global
  | AnfLocal Local
  | AnfConstant Constant
  | AnfIntrinsic Intrinsic

deriving stock instance Eq Anf
deriving stock instance Eq AnfCase
deriving stock instance Eq AnfExpression
deriving stock instance Eq AnfObject
deriving stock instance Eq AnfValue
deriving stock instance Show Anf
deriving stock instance Show AnfCase
deriving stock instance Show AnfExpression
deriving stock instance Show AnfObject
deriving stock instance Show AnfValue
