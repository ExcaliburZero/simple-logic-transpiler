module Transpiler.Translation where

import Transpiler.AST

-- | Transplates the given expression using traditional logic operators into an
-- expression that uses only Sheffer Strokes.
--
-- >>> translate (Not (SProp "A"))
-- Stroke (DProp "A") (DProp "A")
translate :: SExpr -> DExpr
translate (SProp p) = DProp p
translate (And a b) = Stroke (Stroke ad bd) (Stroke ad bd)
  where
    ad = translate a
    bd = translate b
translate (Or  a b) = Stroke (Stroke ad ad) (Stroke bd bd)
  where
    ad = translate a
    bd = translate b
translate (Implies a b) = Stroke ad (Stroke ad bd)
  where
    ad = translate a
    bd = translate b
translate (Not a) = Stroke ad ad
  where
    ad = translate a
