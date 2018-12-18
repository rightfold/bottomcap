-- |
-- Intrinsics.
module Bcapc.Intrinsic
  ( Intrinsic (..)
  ) where

-- |
-- With the exception of the panic intrinsic, all intrinsics must be total.
-- This keeps the language clean from imprecise exceptions.
data Intrinsic
  -- |
  -- The panic intrinsic corresponds to the @std.PANIC@ function.
  = Panic#

deriving stock instance Eq Intrinsic
deriving stock instance Show Intrinsic
