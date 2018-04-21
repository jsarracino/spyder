module Spyder (
  file2Boogie
  , file2Boogiefile
  , hs
) where

import qualified Language.Spyder.AST.Imp as IST
import Language.Boogie.AST as BST
import qualified Language.Spyder.Translate as Translate
import qualified Language.Spyder.Parser as Parser
-- import Text.PrettyPrint.ANSI.Leijen (pretty, hPutDoc)
import Language.Boogie.Pretty (pretty)
import Language.Boogie.PrettyAST ()
import System.Environment (getArgs)
import Control.Monad                        (liftM, liftM2)

file2Boogie :: FilePath -> IO BST.Program
file2Boogie inp = liftM Translate.toBoogie (Parser.fromFile inp)

file2Boogiefile :: FilePath -> FilePath -> IO BST.Program
file2Boogiefile inp outp = do {
  boog <- file2Boogie inp;
  writeFile outp (show $ pretty boog);
  return boog
}

hs = undefined "TODO"
