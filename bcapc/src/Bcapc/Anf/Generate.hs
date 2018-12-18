-- |
-- Embedded domain-specific language for generating administrative normal form.
-- Generating administrative normal form will type check it. It is not possible
-- to construct ill-typed administrative normal form with this module alone.
module Bcapc.Anf.Generate
  ( -- * Monad subclass
    MonadGenerateAnf (..)
  , anfGlobal
  , anfConstant
  , anfIntrinsic

    -- * Monad transformer
  , GenerateAnf
  , GenerateAnfT
  , runGenerateAnf
  , execGenerateAnf
  , runGenerateAnfT
  , execGenerateAnfT
  ) where

import Control.Lens ((.=), (<<+=), use)
import Control.Monad.Error.Class (MonadError, catchError, throwError)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.RWS (RWST (..), runRWST)
import Data.DList (DList)
import Data.Functor.Identity (Identity, runIdentity)
import Data.Traversable (for)
import Data.Word (Word64)

import qualified Control.Monad.Reader.Class as Reader
import qualified Control.Monad.Writer.Class as Writer
import qualified Data.DList as DList
import qualified Data.Vector as Vector

import Bcapc.Anf (Anf (..), AnfCase (..), AnfExpression (..), AnfValue (..))
import Bcapc.Constant (Constant)
import Bcapc.Intrinsic (Intrinsic)
import Bcapc.Name (Field, Global, Local (..))

--------------------------------------------------------------------------------
-- Monad subclass

-- |
-- Those monads that can generate administrative normal form.
class Monad m => MonadGenerateAnf m where
  -- |
  -- Type check and bind an 'AnfApply' expression.
  anfApply :: AnfValue -> AnfValue -> m AnfValue

  -- |
  -- Type check and bind an 'AnfLambda' expression.
  anfLambda :: (AnfValue -> m AnfValue) -> m AnfValue

  -- |
  -- Type check and bind an 'AnfRecord' expression.
  anfRecord :: [(Field, AnfValue)] -> m AnfValue

  -- |
  -- Type check and bind an 'AnfProject' expression.
  anfProject :: Field -> AnfValue -> m AnfValue

  -- |
  -- Type check and bind an 'AnfInject' expression.
  anfInject :: Field -> AnfValue -> m AnfValue

  -- |
  -- Type check and bind an 'AnfMatch' expression.
  anfMatch :: AnfValue -> [(Field, AnfValue -> m AnfValue)] -> m AnfValue

-- |
-- Provided for consistency is this alias for @'pure' . 'AnfGlobal'@.
anfGlobal :: Applicative m => Global -> m AnfValue
anfGlobal = pure . AnfGlobal

-- |
-- Provided for consistency is this alias for @'pure' . 'AnfConstant'@.
anfConstant :: Applicative m => Constant -> m AnfValue
anfConstant = pure . AnfConstant

-- |
-- Provided for consistency is this alias for @'pure' . 'AnfIntrinsic'@.
anfIntrinsic :: Applicative m => Intrinsic -> m AnfValue
anfIntrinsic = pure . AnfIntrinsic

--------------------------------------------------------------------------------
-- Monad transformer

-- |
-- 'GenerateAnfT' with 'Identity'.
type GenerateAnf =
  GenerateAnfT Identity

-- |
-- An implementation of 'MonadGenerateAnf' over any monad.
newtype GenerateAnfT m a =
  GenerateAnfT { unGenerateAnfT :: RWST R W S m a }

----------------------------------------
-- Invocation

type R = ()
type W = DList (Local, AnfExpression)
type S = Word64

-- |
-- 'runGenerateAnfT' with 'Identity'.
runGenerateAnf :: GenerateAnf (a, AnfValue) -> (a, Anf)
runGenerateAnf = runIdentity . runGenerateAnfT

-- |
-- 'execGenerateAnfT' with 'Identity'.
execGenerateAnf :: GenerateAnf AnfValue -> Anf
execGenerateAnf = runIdentity . execGenerateAnfT

-- |
-- Generate administrative normal form.
runGenerateAnfT :: Monad m => GenerateAnfT m (a, AnfValue) -> m (a, Anf)
runGenerateAnfT (GenerateAnfT m) = do
  let s = 1
  ((a, r), _, w) <- runRWST m () s
  pure (a, llFinalize w r)

-- |
-- Like 'runGenerateAnf' but with no extra return value.
execGenerateAnfT :: Monad m => GenerateAnfT m AnfValue -> m Anf
execGenerateAnfT = fmap snd . runGenerateAnfT . fmap ((,) ())

----------------------------------------
-- Low level

llFinalize :: W -> AnfValue -> Anf
llFinalize w r = Anf (Vector.fromList (DList.toList w)) r

llBind :: Monad m => Local -> AnfExpression -> GenerateAnfT m ()
llBind l e = GenerateAnfT . Writer.tell $ DList.singleton (l, e)

llBindValue :: Monad m => AnfExpression -> GenerateAnfT m AnfValue
llBindValue e = do
  l <- llFreshLocal
  llBind l e
  pure $ AnfLocal l

llFreshLocal :: Monad m => GenerateAnfT m Local
llFreshLocal = GenerateAnfT . fmap Synthesized $ id <<+= 1

-- |
-- This is used for bodies of binders such as lambdas. It sets up a new scope
-- in which bindings are emitted, but the generation of fresh names remains the
-- same.
llScope :: Monad m => GenerateAnfT m a -> GenerateAnfT m (a, W)
llScope (GenerateAnfT m) =
  GenerateAnfT $ do
    s <- use id
    (a, s', w) <- lift $ runRWST m () s
    id .= s'
    pure (a, w)

----------------------------------------
-- High level

deriving newtype instance Functor m => Functor (GenerateAnfT m)
deriving newtype instance Monad m => Applicative (GenerateAnfT m)
deriving newtype instance Monad m => Monad (GenerateAnfT m)

instance MonadTrans GenerateAnfT where
  lift m = GenerateAnfT (lift m)

instance Monad m => MonadGenerateAnf (GenerateAnfT m) where
  anfApply function argument =
    -- TODO: Type check the application.
    llBindValue $ AnfApply function argument

  anfLambda lambda = do
    -- TODO: Type check the lambda.
    parameter <- llFreshLocal
    (bodyResult, bodyBindings) <- llScope $ lambda (AnfLocal parameter)
    llBindValue $ AnfLambda parameter (llFinalize bodyBindings bodyResult)

  anfRecord fields = do
    -- TODO: Type check the product.
    llBindValue $ AnfRecord fields

  anfProject field source = do
    -- TODO: Type check the projection.
    llBindValue $ AnfProject field source

  anfInject field source = do
    -- TODO: Type check the injection.
    llBindValue $ AnfInject field source

  anfMatch scrutinee cases = do
    -- TODO: Type check the analysis.
    cases' <- for cases $ \(field, body) -> do
                value <- llFreshLocal
                (bodyResult, bodyBindings) <- llScope $ body (AnfLocal value)
                pure $ AnfCase field value (llFinalize bodyBindings bodyResult)
    llBindValue $ AnfMatch scrutinee cases'

instance MonadError e m => MonadError e (GenerateAnfT m) where
  catchError (GenerateAnfT m) k = GenerateAnfT $ catchError m (unGenerateAnfT . k)
  throwError = lift . throwError

instance MonadReader r m => MonadReader r (GenerateAnfT m) where
  ask = lift Reader.ask
  local f (GenerateAnfT (RWST k)) =
    GenerateAnfT $ RWST $ \() s ->
      Reader.local f (k () s)
