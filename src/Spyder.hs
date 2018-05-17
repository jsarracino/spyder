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
import Data.IORef
import Language.Spyder.Config

import Options.Applicative

file2Boogie :: FilePath -> IO BST.Program
file2Boogie inp = liftM Translate.toBoogie (Parser.fromFile inp)

file2Boogiefile :: FilePath -> FilePath -> IO BST.Program
file2Boogiefile inp outp = do {
  boog <- file2Boogie inp;
  writeFile outp (show $ pretty boog);
  return boog
}

data SpyOptions = SpyOpts { 
    inFile     :: FilePath  -- path to input file
  , outFile    :: FilePath  -- path to output file
  } deriving (Eq, Show, Ord)

runOpts :: SpyOptions -> IO BST.Program
runOpts opts = file2Boogiefile (inFile opts) (outFile opts)


parseOpts :: Parser SpyOptions
parseOpts = SpyOpts <$> inp <*> outp
  where
    inp = strOption ( 
            long "input"
        <>  short 'i'
        <>  metavar "INPUT"
        <> help "Input file location" 
      )
    outp = strOption ( 
            long "output"
        <>  short 'o'
        <>  metavar "OUTPUT"
        <> showDefault
        <> value "out.bpl"
        <> help "Output file location" 
      )
      



hs = runOpts =<< execParser opts
  where
    opts = info (parseOpts <**> helper) ( 
            fullDesc
        <>  progDesc "Compile INPUT Spyder file to OUTPUT Boogie file"
        <>  header "Spyder -- synthesis of web model-view programs" 
      )
  
 
