module Language.Spyder (
  file2Boogie
) where

import qualified Language.Spyder.AST.Imp as IST
import Language.Boogie.AST as BST
import qualified Language.Spyder.Translate as Translate
import qualified Language.Spyder.Parser as Parser
-- import Text.PrettyPrint.ANSI.Leijen (pretty, hPutDoc)
import Language.Boogie.Pretty (pretty)
import Language.Boogie.PrettyAST ()
import System.Environment (getArgs)



file2Boogie :: FilePath -> FilePath -> IO BST.Program
file2Boogie inp outp = do {
  prog <- Parser.fromFile inp;
  let boog = Translate.toBoogie prog in
  do {
    writeFile outp (show $ pretty boog);
    return boog
  }
}

main = undefined "TODO"
