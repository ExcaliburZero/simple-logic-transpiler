module Main where

import Text.Parsec

import Transpiler.AST
import Transpiler.Parsers
import Transpiler.Translation

-- | Takes in an expression from stdin and attempts to transpile it, printing
-- out the results to stdout.
main :: IO ()
main = do
  line    <- getLine
  let dest = transpile line
  _       <- putStrLn (getMessage dest)
  return ()

-- | Attemps to transpile the given string and returns the transpiled String if
-- successful, or the parse error if the parsing failed.
--
-- >>> transpile "!A"
-- Right "(A | A)"
transpile :: String -> Either ParseError String
transpile input = do
  source  <- parse expression input input
  let dest = translate source
  return (toString dest)

-- | Returns the message to display based on the results of the transpiling.
-- Displays the results if sucessful. Else displays the parse error message.
getMessage :: Either ParseError String -> String
getMessage (Right s)  = s
getMessage (Left err) = show err
