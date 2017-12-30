module Transpiler.TranslationSpec (main, spec) where

import Test.Hspec

import Transpiler.AST
import Transpiler.Translation

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec:: Spec
spec = do
  describe "translation" $ do
    it "translate an expression" $ do
      translate sexpr `shouldBe` dexpr
        where
          sexpr = Implies (Not (SProp "A")) (And (SProp "B") (SProp "C"))
          a     = DProp "A"
          b     = DProp "B"
          c     = DProp "C"
          na    = Stroke a a
          and   = Stroke (Stroke b c) (Stroke b c)
          dexpr = Stroke na (Stroke na and)
