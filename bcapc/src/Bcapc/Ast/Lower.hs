{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Convert abstract syntax tree to administrative normal form.
module Bcapc.Ast.Lower
  ( -- * Lowering definitions
    MonadLad
  , LadError (..)
  , LadEnvironment (..)
  , LadT
  , runLadT
  , lowerAstDefinition

    -- * Lowering expressions
  , MonadLae
  , LaeError
  , LaeEnvironment
  , LaeSymbol (..)
  , LaeT
  , runLaeT
  , execLaeT
  , lowerAstExpression
  ) where

import Control.Lens ((^.), (?~), (<>~), at, makeLenses, to, view)
import Control.Monad (join)
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)
import Data.Foldable (fold)
import Data.HashMap.Strict (HashMap)

import qualified Control.Monad.Reader.Class as Reader
import qualified Data.HashMap.Strict as HashMap (fromList)

import Bcapc.Anf.Generate
import Bcapc.Ast

import Bcapc.Anf (Anf, AnfObject (..), AnfValue)
import Bcapc.Name (Field, Global (..), Identifier)

--------------------------------------------------------------------------------
-- Lowering definitions

----------------------------------------
-- Infrastructure

type MonadLad m =
  ( MonadError LadError m
  , MonadReader LadEnvironment m )

data LadEnvironment =
  LadEnvironment
    { -- |
      -- The current package we're in. Every definition we encounter from now
      -- on will be in this package.
      _ladEnvPackage :: [Identifier]

      -- |
      -- All globals, even those not yet traversed. Indexed by package.
    , _ladEnvGlobals :: HashMap [Identifier] [Identifier] }

data LadError
  = Undefined

type LadT m =
  ReaderT LadEnvironment m

runLadT
  :: MonadError LadError m
  => LadT m a
  -> LadEnvironment
  -> m a
runLadT = runReaderT

$(makeLenses ''LadEnvironment)

----------------------------------------
-- Action

lowerAstDefinition :: MonadLad m => AstDefinition -> m [AnfObject]

lowerAstDefinition (AstPackage name definitions) =
  Reader.local (ladEnvPackage <>~ [name]) $
    join <$> traverse lowerAstDefinition definitions

lowerAstDefinition (AstValue name expression) = do
  -- TODO: Consider imports here.
  package <- view ladEnvPackage
  laeEnv <- makeLaeEnv package <$> view ladEnvGlobals
  let name' = Global package name
  expression' <- execLaeT (lowerAstExpression expression) laeEnv
  pure [AnfObject name' expression']
  where
  makeLaeEnv
    :: [Identifier]
    -> HashMap [Identifier] [Identifier]
    -> HashMap Identifier LaeSymbol
  makeLaeEnv p gs =
    let gs' = gs ^. at p . to fold in
    HashMap.fromList
      [ (g, GlobalLaeSymbol (Global p g))
      | g <- gs' ]

--------------------------------------------------------------------------------
-- Lowering expressions

----------------------------------------
-- Infrastructure

type MonadLae m =
  ( MonadGenerateAnf m
  , MonadError LaeError m
  , MonadReader LaeEnvironment m )

type LaeT m =
  GenerateAnfT (ReaderT LaeEnvironment m)

type LaeError =
  LadError

type LaeEnvironment =
  HashMap Identifier LaeSymbol

data LaeSymbol
  = GlobalLaeSymbol Global
  | LocalLaeSymbol AnfValue

runLaeT
  :: MonadError LaeError m
  => LaeT m (a, AnfValue)
  -> LaeEnvironment
  -> m (a, Anf)
runLaeT m e =
  runReaderT (runGenerateAnfT m) e

execLaeT
  :: MonadError LaeError m
  => LaeT m AnfValue
  -> LaeEnvironment
  -> m Anf
execLaeT m e =
  runReaderT (execGenerateAnfT m) e

----------------------------------------
-- Action

lowerAstExpression :: MonadLae m => AstExpression -> m AnfValue

lowerAstExpression (AstVariable name) =
  view (at name) >>= maybe (throwError Undefined) pure >>= \case
    GlobalLaeSymbol  a -> anfGlobal a
    LocalLaeSymbol   a -> pure a

lowerAstExpression (AstApply function argument) = do
  function' <- lowerAstExpression function
  argument' <- lowerAstExpression argument
  anfApply function' argument'

lowerAstExpression (AstLambda parameter body) =
  anfLambda $ \parameter' ->
    let parameter'' = LocalLaeSymbol parameter' in
    Reader.local (at parameter ?~ parameter'') $
      lowerAstExpression body

lowerAstExpression (AstConstant constant) =
  anfConstant constant

lowerAstExpression (AstIntrinsic intrinsic) =
  anfIntrinsic intrinsic

lowerAstExpression (AstRecord fields) = do
  fields' <- traverse (traverse lowerAstExpression) fields
  anfRecord fields'

lowerAstExpression (AstProject field source) = do
  source' <- lowerAstExpression source
  anfProject field source'

lowerAstExpression (AstInject field value) = do
  value' <- lowerAstExpression value
  anfInject field value'

lowerAstExpression (AstMatch scrutinee cases) = do
  scrutinee' <- lowerAstExpression scrutinee
  anfMatch scrutinee' (lowerAstCase <$> cases)

lowerAstCase :: MonadLae m => AstCase -> (Field, AnfValue -> m AnfValue)
lowerAstCase (AstCase field value body) =
  ( field
  , \value' ->
      let value'' = LocalLaeSymbol value' in
      Reader.local (at value ?~ value'') $
        lowerAstExpression body )

--------------------------------------------------------------------------------
-- Micellaneous

deriving stock instance Show LaeError
