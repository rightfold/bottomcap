module Bcapc.Anf.TestData where

import Bcapc.Anf (AnfValue (..))
import Bcapc.Constant (Constant (..))
import Bcapc.Name (Global (..), Local (..))

--------------------------------------------------------------------------------
-- Globals

aglo1 :: AnfValue
aglo1 = AnfGlobal glo1

glo1 :: Global
glo1 = Global [] "1"

--------------------------------------------------------------------------------
-- Locals

asyn1, asyn2, asyn3, asyn4, asyn5 :: AnfValue
asyn1 = AnfLocal syn1
asyn2 = AnfLocal syn2
asyn3 = AnfLocal syn3
asyn4 = AnfLocal syn4
asyn5 = AnfLocal syn5

syn1, syn2, syn3, syn4, syn5 :: Local
syn1 = Synthesized 1
syn2 = Synthesized 2
syn3 = Synthesized 3
syn4 = Synthesized 4
syn5 = Synthesized 5

--------------------------------------------------------------------------------
-- Constants

atrue :: AnfValue
atrue = AnfConstant true

true :: Constant
true = BoolConstant True
