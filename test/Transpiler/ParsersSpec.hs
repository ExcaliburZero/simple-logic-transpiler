module Transpiler.ParsersSpec (main, spec) where

import Test.Hspec

import Text.Parsec

import Transpiler.AST
import Transpiler.Parsers

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec:: Spec
spec = do
  describe "proposition" $ do
    it "parse a valid proposition" $ do
      parse proposition "A" "A" `shouldBe` Right (SProp "A")

  describe "negation" $ do
    it "parse a valid negation" $ do
      parse negation "!A" "!A" `shouldBe` Right (Not (SProp "A"))

  describe "binaryOperation" $ do
    it "parse a valid and expression" $ do
      parse binaryOperation "(A * B)" "(A * B)" `shouldBe`
        Right (And (SProp "A") (SProp "B"))
    it "parse a valid or expression" $ do
      parse binaryOperation "(A v B)" "(A v B)" `shouldBe`
        Right (Or (SProp "A") (SProp "B"))
    it "parse a valid implication expression" $ do
      parse binaryOperation "(A -> B)" "(A -> B)" `shouldBe`
        Right (Implies (SProp "A") (SProp "B"))

  describe "expression" $ do
    it "parse a valid expression" $ do
      parse expression "!(A * !B)" "!(A * !((B v B) -> C))" `shouldBe`
        Right (Not (And (SProp "A") (Not (Implies (Or (SProp "B") (SProp "B"))
            (SProp "C")))))
