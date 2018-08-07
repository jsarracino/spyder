module Language.Spyder (
  file2Boogie
  , file2Boogiefile
  , hs
  , main
) where

import qualified Language.Spyder.AST.Imp as IST
import Language.Boogie.AST as BST
import qualified Language.Spyder.Translate as Translate
import qualified Language.Spyder.Parser as Parser
-- import Text.PrettyPrint.ANSI.Leijen (pretty, hPutDoc)
import Language.Boogie.Pretty (pretty)
import Language.Boogie.PrettyAST ()
import System.Environment (getArgs)
import Control.Monad                        (liftM, liftM2, void)
import Data.IORef
import Language.Spyder.Config

import Language.Spyder.Bench

import Options.Applicative
import Options.Applicative.Types

-- maybeReader :: (String -> Maybe a) -> ReadM a
-- maybeReader f = do
--   arg  <- readerAsk
--   maybe (readerError $ "cannot parse value `" ++ arg ++ "'") return . f $ arg

file2Boogie :: FilePath -> IO BST.Program
file2Boogie inp = liftM Translate.toBoogie (Parser.fromFile inp)

file2Boogiefile :: FilePath -> FilePath -> IO BST.Program
file2Boogiefile inp outp = do {
  boog <- file2Boogie inp;
  writeFile outp (show $ pretty boog);
  return boog
}

maybeReader :: (String -> Maybe a) -> ReadM a
maybeReader f = eitherReader $ \arg ->
  maybe (Left $ "cannot parse value `" ++ arg ++ "'") pure . f $ arg

data BenchMode = Boog | Spy | Invs
  deriving (Eq, Show, Ord)

data SpyOptions = SpyOpts { 
        inFile       :: FilePath   -- path to input file
      , outFile      :: FilePath   -- path to output file
    } 
  | Benches {
      ty              :: BenchMode
    , inp             :: FilePath
  }
  deriving (Eq, Show, Ord)

  -- spyder_source_size = re.match("\(source: size (\d+)\).*$",spy_contents).group(1)
  -- spyder_invariant_size = re.match("\(inv: size (\d+)\).*$",spy_contents).group(1)
  -- spyder_invariant_size = re.match("\(lines: (\d+)\).*$",spy_contents).group(1)
  -- spyder_holes = len(re.findall("\(holes: (\d+)\).*$",spy_contents))
runOpts :: SpyOptions -> IO ()
runOpts (SpyOpts inf outf) = do {
  -- putStrLn $ "source: size " ++ show $ boogSize boog;
  spysize <- liftM spySize spy;
  putStrLn $ "source: size " ++ show spysize;
  invsize <- liftM invSize spy;
  putStrLn $ "inv: size " ++ show invsize;
  file2Boogiefile inf outf;
  return ()
}
  where
    boog = file2Boogie inf
    spy = Parser.fromFile inf
runOpts (Benches mode inf) = do {
    i <- worker mode;
    putStrLn $ "result : " ++ show i
  }
  where
    boog = file2Boogie inf
    spy = Parser.fromFile inf

    worker :: BenchMode -> IO Int
    worker Boog   = liftM boogSize boog
    worker Spy    = liftM spySize spy
    worker Invs  = liftM invSize spy


parseOpts :: Parser SpyOptions
parseOpts = normal <|> benches
  where
    benches :: Parser SpyOptions
    benches = Benches <$> bmode <*> inp
    normal = SpyOpts <$> inp <*> outp
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

      
    bmode :: Parser BenchMode
    bmode = option (maybeReader makeBench) (long "bench"
        <>  short 'b'
        <>  metavar "BENCH"
        <> help "Benchmark switch, followed by `Boog` or `Spy` or `Inv`." 
      )

    makeBench "Boog" = Just Boog
    makeBench "Spy" = Just Spy
    makeBench "Inv" = Just Invs
    makeBench _ = Nothing
      



hs = runOpts =<< execParser opts
  where
    opts = info (parseOpts <**> helper) ( 
            fullDesc
        <>  progDesc "Compile INPUT Spyder file to OUTPUT Boogie file"
        <>  header "Spyder -- synthesis of web model-view programs" 
      )
  
 
main = hs