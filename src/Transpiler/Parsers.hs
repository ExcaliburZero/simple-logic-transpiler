module Transpiler.Parsers where

import Text.Parsec.Char
import Text.ParserCombinators.Parsec

import Transpiler.AST

-- | A function that can be used to contruct a binary operation expression.
type BinOpConstructor = SExpr -> SExpr -> SExpr

-- | Parses a proposition.
proposition :: Parser SExpr
proposition = do
  name <- upper
  return (SProp [name])

-- | Parses a negation operation.
negation :: Parser SExpr
negation = do
  _ <- string "!"
  e <- expression
  return (Not e)

-- | Parses a unary operation.
unaryOperation :: Parser SExpr
unaryOperation = negation

-- | Parses an and operator and returns a function for creating an And
-- expression.
andOp :: Parser BinOpConstructor
andOp = do
  _ <- string "*"
  return And

-- | Parses an or operator and returns a function for creating an Or
-- expression.
orOp :: Parser BinOpConstructor
orOp = do
  _ <- string "v"
  return Or

-- | Parses an implication operator and returns a function for creating an
-- Implies expression.
impliesOp :: Parser BinOpConstructor
impliesOp = do
  _ <- string "->"
  return Implies

-- | Parses a unary operator and returns a function for creating the
-- corresponding expression.
binaryOperator :: Parser BinOpConstructor
binaryOperator = andOp <|> orOp <|> impliesOp

-- | Parses a binary operation.
binaryOperation :: Parser SExpr
binaryOperation = do
  _  <- string "("
  a  <- expression
  _  <- space
  op <- binaryOperator
  _  <- space
  b  <- expression
  _  <- string ")"
  return (op a b)

-- | Parses a propositional logic expression that uses only traditional logic
-- operators.
expression :: Parser SExpr
expression = 
      proposition
  <|> unaryOperation
  <|> binaryOperation
