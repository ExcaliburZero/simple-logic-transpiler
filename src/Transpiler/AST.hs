module Transpiler.AST where

-- | A propositional logic expression that uses only traditional logic
-- operators (and, or, implication, and negation).
data SExpr =
    And SExpr SExpr
  | Or SExpr SExpr
  | Implies SExpr SExpr
  | Not SExpr
  | SProp String
  deriving (Eq, Show)

-- | A propositional logic expression that uses only Sheffer Stroke operators.
data DExpr =
    Stroke DExpr DExpr
  | DProp String
  deriving (Eq, Show)

-- | Converts the given Sheffer Stroke expression into a String representation.
--
-- >>> toString (Stroke (DProp "A") (DProp "B"))
-- "(A | B)"
toString :: DExpr -> String
toString (DProp p) = p
toString (Stroke a b) = "(" ++ as ++ " | " ++ bs ++ ")"
  where
    as = toString a
    bs = toString b
