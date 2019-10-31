module Language.Spyder (
  file2Boogie
  , file2Boogiefile
  , hs
  , main
) where

import qualified Language.Spyder.AST.Imp as IST
import qualified Language.Spyder.AST as SST
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

file2Boogie :: Bool -> FilePath -> IO BST.Program
file2Boogie plain inp = liftM (Translate.toBoogie plain) (Parser.fromFile inp)

file2Boogiefile :: FilePath -> FilePath -> IO BST.Program
file2Boogiefile inp outp = do {
  boog <- file2Boogie False inp;
  writeFile outp (show $ pretty boog);
  return boog
}

file2Spy :: FilePath -> IO SST.Program
file2Spy = Parser.fromFile

file2PlainBoogie :: FilePath -> FilePath -> IO BST.Program
file2PlainBoogie inp outp = do {
  boog <- file2Boogie True inp;
  writeFile outp (show $ pretty boog);
  return boog
}

-- maybeReader :: (String -> Maybe a) -> ReadM a
-- maybeReader f = eitherReader $ \arg ->
--   maybe (Left $ "cannot parse value `" ++ arg ++ "'") pure . f $ arg

data BenchMode = Boog | Spy | Invs
  deriving (Eq, Show, Ord)

data SpyOptions = 
    Compile { 
      inFile       :: FilePath   -- path to input file
    , outFile      :: FilePath   -- path to output file
    } 
  | BenchSize {
      ty              :: BenchMode
    , inp             :: FilePath
    }
  | BenchDiff {
      inpF             :: FilePath
    , outF             :: FilePath
    } 
  | Parse {
      fname :: FilePath
    }
  deriving (Eq, Show, Ord)

  -- spyder_source_size = re.match("\(source: size (\d+)\).*$",spy_contents).group(1)
  -- spyder_invariant_size = re.match("\(inv: size (\d+)\).*$",spy_contents).group(1)
  -- spyder_invariant_size = re.match("\(lines: (\d+)\).*$",spy_contents).group(1)
  -- spyder_holes = len(re.findall("\(holes: (\d+)\).*$",spy_contents))
runOpts :: SpyOptions -> IO ()
runOpts (Compile inf outf) = do {
  -- putStrLn $ "source: size " ++ show $ boogSize boog;
  -- implsize <- liftM implSize spy;
  -- putStrLn $ "impl: size " ++ show implsize;
  -- specsize <- liftM specSize spy;
  -- putStrLn $ "spec: size " ++ show specsize;
  -- file2PlainBoogie inf "plain.bpl";
  file2Boogiefile inf outf;
  return ()
}
  where
    spy = Parser.fromFile inf
runOpts (BenchSize _ inf) = do {
    implsize <- liftM implSize spy;
    putStrLn $ "impl: size " ++ show implsize;
    specsize <- liftM specSize spy;
    putStrLn $ "spec: size " ++ show specsize;
  }
  where
    spy = Parser.fromFile inf

runOpts (Parse inf) = putStrLn =<< Parser.tryParsing <$> readFile inf 
runOpts (BenchDiff inp outp) = do {
  inf <- Parser.fromFile inp;
  writeFile (outp ++ "-spec") $ show $ specTree inf;
  writeFile (outp ++ "-impl") $ show $ implTree inf;
  writeFile outp $ show $ treeify inf;
}

parseOpts :: Parser SpyOptions
parseOpts = normal <|> (df *> diff) <|> sizebench <|> parse 
  where
    sizebench :: Parser SpyOptions
    sizebench = BenchSize <$> bmode <*> inp
    normal = Compile <$> inp <*> outp
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

    parse :: Parser SpyOptions
    parse = Parse <$> fname 
    fname = strOption (
            long "parse input"
        <>  short 'p'
        <>  metavar "INPUT"
        <>  help "Input file location"
      )
    diff :: Parser SpyOptions
    diff = BenchDiff <$> inp <*> outp
    df = switch (long "diff" <> short 'd' <> help "Diff switch")

      
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