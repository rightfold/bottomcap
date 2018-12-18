{-# LANGUAGE StrictData #-}

-- |
-- Names of items in programs. These are frequently used as hash map keys, so
-- we cache their hashes inside them.
module Bcapc.Name
  ( Identifier (..)
  , Global (..)
  , Local (..)
  , Field (..)
  ) where

import Data.Hashable (Hashable, Hashed, hashed, unhashed)
import Data.Serialize (Serialize, get, put)
import Data.String (IsString)
import Data.Text (Text)
import Data.Word (Word64)
import GHC.Generics (Generic)

import qualified Data.Text as Text

newtype Identifier =
  Identifier (Hashed Text)

-- |
-- bcapc.name.global.
data Global =
  Global
    { globalPackage :: [Identifier]
    , globalName    :: Identifier }

-- |
-- bcapc.name.local.
data Local
  = Local Identifier
  | Synthesized Word64

-- |
-- bcapc.name.field.
newtype Field =
  Field Identifier

deriving anyclass instance Hashable Field
deriving anyclass instance Hashable Global
deriving anyclass instance Hashable Local
deriving anyclass instance Serialize Field
deriving anyclass instance Serialize Global
deriving anyclass instance Serialize Local
deriving newtype instance Hashable Identifier
deriving newtype instance IsString Identifier
deriving stock instance Eq Field
deriving stock instance Eq Global
deriving stock instance Eq Identifier
deriving stock instance Eq Local
deriving stock instance Generic Field
deriving stock instance Generic Global
deriving stock instance Generic Identifier
deriving stock instance Generic Local
deriving stock instance Show Field
deriving stock instance Show Global
deriving stock instance Show Identifier
deriving stock instance Show Local

instance Serialize Identifier where
  put (Identifier (unhashed -> Text.unpack -> name)) = put name
  get = Identifier . hashed . Text.pack <$> get
