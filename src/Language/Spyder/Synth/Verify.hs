module Language.Spyder.Synth.Verify (
    checkProgFile
  , debugBoogie
  , checkProg
) where

import System.Process
import System.Exit
-- import Language.Spyder.AST          (Decl, Program)

import Language.Boogie.Pretty                     (pretty)
import Language.Boogie.AST
import Language.Boogie.PrettyAST
import System.IO.Unsafe                           (unsafePerformIO)

-- unsafe, oops
checkProg :: Program -> Bool
checkProg p = unsafePerformIO $ compileProg p >> checkProgFile "debug.bpl" 

checkProgFile :: FilePath -> IO Bool
checkProgFile f = do {
  (_, _, _, ph) <- createProcess $ shell cmd;
  r <- waitForProcess ph;
  return $ r /= ExitSuccess
} where cmd     = prefix ++ " && ./boog.sh " ++ f ++ " | grep Error"
        prefix  = "echo \" CHECKING BOOGIE AT " ++ f ++ "\":"

debugBoogie :: [Decl] -> [Decl]
debugBoogie p = unsafePerformIO $ compileProg (Program p) >> return p
  where
    outp = "debug.bpl"

compileProg :: Program -> IO ()
compileProg p = writeFile "debug.bpl" (show $ pretty $ p)