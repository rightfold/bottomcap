{-# LANGUAGE StrictData #-}

-- |
-- Constants are immutable values that can be embedded inside executables.
module Bcapc.Constant
  ( Constant (..)
  ) where

import Data.Hashable (Hashable)
import GHC.Generics (Generic)

-- |
-- Constant.
data Constant
  = BoolConstant Bool

deriving anyclass instance Hashable Constant
deriving stock instance Eq Constant
deriving stock instance Generic Constant
deriving stock instance Ord Constant
deriving stock instance Show Constant
