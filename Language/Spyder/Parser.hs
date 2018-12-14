module Language.Spyder.Parser (
    module Lexer
  , module Parser
  , str2Expr
  , str2Stmt
  , str2Prog
  , str2Block
  , str2Dec
  , str2Rel
  , str2Comp
  , fromFile
  , fromBoogieFile
  , fromBoogUS
) where
import Language.Spyder.AST                                    (Program)
import Language.Spyder.AST.Spec
import qualified Language.Spyder.Parser.Lexer as Lexer
import qualified Language.Spyder.Parser.Parser as Parser
import Text.Parsec
import Control.Monad (liftM)

import qualified Language.Boogie.AST as BST
import qualified Language.Boogie.Parser as BP

import System.IO.Unsafe                                         (unsafePerformIO)

-- TODO: there's a parse error when atoms are preceeded by a newline

str2A :: (Show a) => Lexer.Parser a -> String -> a
str2A p s = case parse p "" s of
  Right res -> res
  Left msg -> error $ "couldn't parse because " ++ show msg

str2Expr = str2A Parser.expr
str2Stmt = str2A Parser.stmt
str2Prog = str2A Parser.prog
str2Block = str2A Parser.block
str2Dec = str2A Parser.relDeclP
str2Rel = str2A Parser.relP
str2Comp = str2A Parser.comp

fromFile :: FilePath -> IO Program
fromFile inp = liftM str2Prog (readFile inp)

fromBoogieFile :: FilePath -> IO BST.Program
fromBoogieFile inp = liftM (str2A BP.program) (readFile inp)

fromBoogUS inp = unsafePerformIO $ fromBoogieFile inp