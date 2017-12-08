module Language.Spyder.Parser (
    module Lexer
  , module Parser
  , str2Expr
  , str2Stmt
  , str2Prog
) where

import qualified Language.Spyder.Parser.Lexer as Lexer
import qualified Language.Spyder.Parser.Parser as Parser
import Text.Parsec


str2A :: (Show a) => Lexer.Parser a -> String -> a
str2A p s = case parse p "" s of
  Right res -> res
  Left msg -> undefined $ "couldn't parse: " ++ s ++ " because " ++ (show msg)

str2Expr = str2A Parser.expr
str2Stmt = str2A Parser.stmt
str2Prog = str2A Parser.prog
