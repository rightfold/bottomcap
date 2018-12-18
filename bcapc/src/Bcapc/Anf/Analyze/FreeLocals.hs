-- |
-- Compute the free locals of administrative normal form.
module Bcapc.Anf.Analyze.FreeLocals
  ( anfFreeLocals
  , anfExpressionFreeLocals
  , anfValueFreeLocals
  ) where

import Prelude hiding (init)

import Data.HashSet (HashSet)
import Data.Semigroup ((<>))

import qualified Data.HashSet as HashSet

import Bcapc.Anf

import Bcapc.Name (Local)

-- |
-- Compute the free locals of administrative normal form.
anfFreeLocals :: Anf -> HashSet Local
anfFreeLocals (Anf bindings result) =
  let
    init :: HashSet Local
    init = anfValueFreeLocals result

    step :: (Local, AnfExpression) -> HashSet Local -> HashSet Local
    step (local, expression) body =
      HashSet.delete local body <>
      anfExpressionFreeLocals expression
  in
    foldr step init bindings

-- |
-- Compute the free locals of an expression.
anfExpressionFreeLocals :: AnfExpression -> HashSet Local
anfExpressionFreeLocals (AnfApply a b) = anfValueFreeLocals a <> anfValueFreeLocals b
anfExpressionFreeLocals (AnfLambda a b) = HashSet.delete a (anfFreeLocals b)
anfExpressionFreeLocals (AnfRecord a) = foldMap (anfValueFreeLocals . snd) a
anfExpressionFreeLocals (AnfProject _ a) = anfValueFreeLocals a
anfExpressionFreeLocals (AnfInject _ a) = anfValueFreeLocals a
anfExpressionFreeLocals (AnfMatch a b) = anfValueFreeLocals a <> foldMap (anfFreeLocals . anfCaseBody) b

-- |
-- Compute the free locals of a value.
anfValueFreeLocals :: AnfValue -> HashSet Local
anfValueFreeLocals (AnfGlobal _)    = HashSet.empty
anfValueFreeLocals (AnfLocal a)     = HashSet.singleton a
anfValueFreeLocals (AnfConstant _)  = HashSet.empty
anfValueFreeLocals (AnfIntrinsic _) = HashSet.empty
