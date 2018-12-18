{-# LANGUAGE TemplateHaskell #-}

-- |
-- Linking ECMAScript objects produces an executable. The linking process works
-- according to the following rules:
--
--  1. If two objects have the same name, fail.
--  2. If not all dependencies of all objects were defined, fail.
--  3. Concatenate the code of all objects and return it.
module Bcapc.Target.EcmaScript.Link
  ( EcmaScriptLinkError (..)
  , linkEcmaScriptObjects
  , linkEcmaScriptObjects'
  ) where

import Control.Lens ((?=), _1, _2, at, makePrisms, use)
import Control.Monad (when)
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.State.Class (MonadState)
import Control.Monad.Trans.State (evalStateT)
import Data.ByteString.Builder (Builder)
import Data.ByteString.Lazy (ByteString)
import Data.Foldable (fold, traverse_)
import Data.HashSet (HashSet)
import Data.Traversable (for)

import qualified Data.ByteString.Builder as Builder
import qualified Data.HashSet as HashSet

import Bcapc.Name (Global)
import Bcapc.Target.EcmaScript.Object (EcmaScriptObject (..))

newtype Definitions = Definitions (HashSet Global)
newtype Dependencies = Dependencies (HashSet Global)
$(makePrisms ''Definitions)
$(makePrisms ''Dependencies)

data EcmaScriptLinkError
  = Redefinition Global
  | Undefined

deriving stock instance Eq EcmaScriptLinkError
deriving stock instance Show EcmaScriptLinkError

-- |
-- Link ECMAScript objects to produce an executable. A set of exports may also
-- be provided; linking will ensure that all of these have been defined. This
-- is useful for ensuring the entry point exists.
linkEcmaScriptObjects
  :: [EcmaScriptObject]
  -> HashSet Global
  -> Either EcmaScriptLinkError ByteString
linkEcmaScriptObjects objects exports =
  fmap Builder.toLazyByteString $
    linkEcmaScriptObjects' objects exports

-- |
-- Like 'linkEcmaScriptObjects', but produce a 'Builder'.
linkEcmaScriptObjects'
  :: [EcmaScriptObject]
  -> HashSet Global
  -> Either EcmaScriptLinkError Builder
linkEcmaScriptObjects' objects exports =
  evalStateT (linkEcmaScriptObjects'' objects)
             ( Definitions HashSet.empty
             , Dependencies exports )

linkEcmaScriptObjects''
  :: ( MonadError EcmaScriptLinkError m
     , MonadState (Definitions, Dependencies) m )
  => [EcmaScriptObject] -> m Builder
linkEcmaScriptObjects'' os = do
  codes <- for os $ \(EcmaScriptObject objName objDependencies objCode) -> do
             recordDefinition objName
             traverse_ recordDependency objDependencies
             pure $ Builder.byteString objCode
  checkDependenciesDefined
  pure $ fold codes

-- |
-- Record a definition.
recordDefinition
  :: ( MonadError EcmaScriptLinkError m
     , MonadState (Definitions, s) m )
  => Global -> m ()
recordDefinition name = do
  checkRedefinition name
  _1 . _Definitions . at name ?= ()

-- |
-- Check that a definition was not already defined.
checkRedefinition
  :: ( MonadError EcmaScriptLinkError m
     , MonadState (Definitions, s) m )
  => Global -> m ()
checkRedefinition name = do
  definitions <- use $ _1 . _Definitions
  when (HashSet.member name definitions) $
    throwError $ Redefinition name

-- |
-- Record a dependency.
recordDependency
  :: MonadState (s, Dependencies) m
  => Global -> m ()
recordDependency name =
  _2 . _Dependencies . at name ?= ()

-- |
-- Check that all dependencies were defined.
checkDependenciesDefined
  :: ( MonadError EcmaScriptLinkError m
     , MonadState (Definitions, Dependencies) m )
  => m ()
checkDependenciesDefined = do
  definitions  <- use $ _1 . _Definitions
  dependencies <- use $ _2 . _Dependencies
  when (any (not . flip HashSet.member definitions) dependencies) $
    throwError Undefined
