module Bcapc.Main.Bcapc
  ( main
  ) where

import Control.Lens ((&))
import Control.Monad.Trans.Except (runExceptT)

import qualified Data.ByteString.Lazy as ByteString

import Bcapc.Ast
import Bcapc.Ast.Lower

import Bcapc.Constant (Constant (..))
import Bcapc.Intrinsic (Intrinsic (..))
import Bcapc.Name (Field (..))
import Bcapc.Target.EcmaScript.Link (linkEcmaScriptObjects)
import Bcapc.Target.EcmaScript.Translate (translateAnfObject)

main :: IO ()
main = do
  let env = LadEnvironment [] [([], ["example1", "example2", "example3"])]
  example1' <- runExceptT (runLadT (lowerAstDefinition example1) env) >>= either (fail . show) pure
  example2' <- runExceptT (runLadT (lowerAstDefinition example2) env) >>= either (fail . show) pure
  example3' <- runExceptT (runLadT (lowerAstDefinition example3) env) >>= either (fail . show) pure
  let examples = example1' <> example2' <> example3'
  examples' <- traverse translateAnfObject examples
  examples'' <- linkEcmaScriptObjects examples' [] & either (fail . show) pure
  ByteString.putStr examples'' *> putStrLn ""

example1 :: AstDefinition
example1 =
  AstValue "example1" $
    AstLambda "argument" $
      AstApply (AstIntrinsic Panic#)
               (AstVariable "argument")

example2 :: AstDefinition
example2 =
  AstValue "example2" $
    AstLambda "argument" $
      AstApply (AstMatch (AstVariable "example1")
                         [ AstCase (Field "some") "v" (AstVariable "v")
                         , AstCase (Field "none") "_" (AstConstant (BoolConstant False)) ])
               (AstVariable "argument")

example3 :: AstDefinition
example3 =
  AstValue "example3" $
    AstLambda "argument" $
      AstApply (AstVariable "example2")
               (AstConstant (BoolConstant True))
