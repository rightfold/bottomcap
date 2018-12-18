-- |
-- Translate administrative normal form into ECMAScript.
module Bcapc.Target.EcmaScript.Translate
  ( -- * Infrastructure
    TranslateAnf
  , TranslateAnfT
  , runTranslate
  , runTranslateT

    -- * Translate administrative normal form
  , translateAnfObject
  , translateAnf
  , translateAnfExpression
  , translateAnfValue

    -- * Translate constants
  , translateConstant

    -- * Translate intrinsics
  , translateIntrinsic
  ) where

import Control.Monad.Trans.Writer (WriterT, runWriterT)
import Data.Bool (bool)
import Data.ByteString.Builder (Builder)
import Data.Foldable (fold)
import Data.Functor.Identity (Identity, runIdentity)
import Data.HashSet (HashSet)
import Data.Traversable (for)

import qualified Control.Monad.Writer.Class as Writer
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.HashSet as HashSet

import Bcapc.Anf

import Bcapc.Constant (Constant (..))
import Bcapc.Intrinsic (Intrinsic (..))
import Bcapc.Name (Global, Local)
import Bcapc.Name.Mangle (mangleField, mangleGlobal, mangleLocal)
import Bcapc.Target.EcmaScript.Object (EcmaScriptObject (..))

--------------------------------------------------------------------------------
-- Infrastructure

type TranslateAnf =
  TranslateAnfT Identity

newtype TranslateAnfT m a =
  TranslateAnfT (WriterT (HashSet Global) m a)

runTranslate :: TranslateAnf a -> (a, HashSet Global)
runTranslate = runIdentity . runTranslateT

runTranslateT :: TranslateAnfT m a -> m (a, HashSet Global)
runTranslateT (TranslateAnfT m) = runWriterT m

--------------------------------------------------------------------------------
-- Translate administrative normal form

translateAnfObject :: Monad m => AnfObject -> m EcmaScriptObject
translateAnfObject (AnfObject global anf) = do
  (result, dependencies) <- runTranslateT $ translateAnf anf
  let code = "var " <> mangleGlobal global <> " = function() {\n" <>
             result "bcaprResult" <>
             mangleGlobal global <> " = function() {\n" <>
             "return bcaprResult;\n" <>
             "};\n" <>
             "return bcaprResult;\n" <>
             "};\n"
  let code' = ByteString.toStrict . Builder.toLazyByteString $ code
  pure $ EcmaScriptObject global dependencies code'

-- |
-- Translate administrative normal form into a sequence of ECMAScript
-- statements, ultimately assigning the result of the expression to the given
-- target.
translateAnf :: forall m. Monad m => Anf -> TranslateAnfT m (Builder -> Builder)
translateAnf (Anf bindings result) = do
  bindings' <- fold <$> traverse translateAnfExpression' bindings
  result' <- translateAnfValue result
  pure $ \r -> bindings' <> "var " <> r <> " = " <> result' <> ";\n"
  where
  translateAnfExpression' :: (Local, AnfExpression) -> TranslateAnfT m Builder
  translateAnfExpression' (local, expression) =
    ($ mangleLocal local) <$>
      translateAnfExpression expression

----------------------------------------
-- Translate expressions

-- |
-- Translate an expression into a sequence of ECMAScript statements, ultimately
-- assigning the result of the expression to the given target.
translateAnfExpression :: Monad m => AnfExpression -> TranslateAnfT m (Builder -> Builder)

translateAnfExpression (AnfApply function argument) = do
  function' <- translateAnfValue function
  argument' <- translateAnfValue argument
  pure $ \r -> "var " <> r <> " = " <> function' <> "(" <> argument' <> ");\n"

translateAnfExpression (AnfLambda parameter body) = do
  let parameter' = mangleLocal parameter
  body' <- translateAnf body
  pure $ \r -> "var " <> r <> " = function(" <> parameter' <> ") {\n" <>
                 body' "bcaprResult" <> "return bcaprResult;\n" <>
               "};\n"

translateAnfExpression (AnfRecord fields) = do
  fields' <- for fields $ \(field, value) -> do
               value' <- translateAnfValue value
               pure $ mangleField field <> ": " <> value' <> ",\n"
  pure $ \r -> "var " <> r <> " = {\n" <> fold fields' <> "};\n"

translateAnfExpression (AnfProject field value) = do
  value' <- translateAnfValue value
  pure $ \r -> "var " <> r <> " = " <> value' <> "." <> mangleField field <> ";\n"

translateAnfExpression (AnfInject field value) = do
  value' <- translateAnfValue value
  pure $ \r -> "var " <> r <> " = ['" <> mangleField field <> "', " <> value' <> "];\n"

translateAnfExpression (AnfMatch scrutinee cases) = do
  scrutinee' <- translateAnfValue scrutinee
  cases' <- traverse translateAnfCase cases
  pure $ \r -> "switch (" <> scrutinee' <> "[0]) {\n" <>
               fold [ c scrutinee' r | c <- cases' ] <>
               "default:\n" <>
               "var " <> r <> " = null;\n" <>
               "break;\n" <>
               "}\n"

translateAnfCase :: Monad m => AnfCase -> TranslateAnfT m (Builder -> Builder -> Builder)
translateAnfCase (AnfCase field value body) = do
  body' <- translateAnf body
  pure $ \s r -> "case '" <> mangleField field <> "':\n" <>
                 "var " <> mangleLocal value <> " = " <> s <> "[1];\n" <>
                 body' r <>
                 "break;\n"

----------------------------------------
-- Translate values

-- |
-- Translate a value into an ECMAScript expression.
translateAnfValue :: Monad m => AnfValue -> TranslateAnfT m Builder

translateAnfValue (AnfGlobal name) =
  TranslateAnfT $ do
    Writer.tell $ HashSet.singleton name
    pure $ mangleGlobal name <> "()"

translateAnfValue (AnfLocal name) =
  pure $ mangleLocal name

translateAnfValue (AnfConstant constant) =
  translateConstant constant

translateAnfValue (AnfIntrinsic intrinsic) =
  translateIntrinsic intrinsic

--------------------------------------------------------------------------------
-- Translate constants

-- |
-- Translate a constant into an ECMAScript expression.
translateConstant :: Applicative m => Constant -> m Builder

translateConstant (BoolConstant value) =
  pure $ bool "false" "true" value

--------------------------------------------------------------------------------
-- Translate intrinsics

-- |
-- Translate an intrinsic into an ECMAScript expression.
translateIntrinsic :: Applicative m => Intrinsic -> m Builder
translateIntrinsic Panic# = pure "bcaprPanic"

--------------------------------------------------------------------------------
-- Miscellaneous

deriving newtype instance Functor m => Functor (TranslateAnfT m)
deriving newtype instance Monad m => Applicative (TranslateAnfT m)
deriving newtype instance Monad m => Monad (TranslateAnfT m)
