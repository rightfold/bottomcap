{-# LANGUAGE StrictData #-}

module Bcapc.Ast
  ( AstDefinition (..)
  , AstExpression (..)
  , AstCase (..)
  ) where

import Bcapc.Constant (Constant)
import Bcapc.Intrinsic (Intrinsic)
import Bcapc.Name (Field, Identifier)

-- |
-- A package-level definition.
data AstDefinition
  -- |
  -- Definition of a package.
  = AstPackage Identifier [AstDefinition]

  -- |
  -- Definition of a value.
  | AstValue Identifier AstExpression

data AstExpression
  -- |
  -- Reference a local, global, or package. If a package is referenced, this
  -- must be the operand to 'AstProject'.
  = AstVariable Identifier

  -- |
  -- Apply a function to a value.
  | AstApply AstExpression AstExpression

  -- |
  -- Construct a lambda abstraction.
  | AstLambda Identifier AstExpression

  -- |
  -- Constant value.
  | AstConstant Constant

  -- |
  -- Built-in value.
  | AstIntrinsic Intrinsic

  -- |
  -- Construct a record value.
  | AstRecord [(Field, AstExpression)]

  -- |
  -- Project a field from a record value.
  | AstProject Field AstExpression

  -- |
  -- Inject a value into a variant value.
  | AstInject Field AstExpression

  -- |
  -- Analyze a variant value.
  | AstMatch AstExpression [AstCase]

data AstCase =
  AstCase
    { astCaseField :: Field
    , astCaseValue :: Identifier
    , astCaseBody  :: AstExpression }
