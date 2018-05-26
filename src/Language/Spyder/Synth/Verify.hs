module Language.Spyder.Synth.Verify (
    checkProgFile
  , debugBoogie
  , debugBlock
  , checkProg
  , debugProg
) where

import System.Process
import System.Exit
-- import Language.Spyder.AST          (Decl, Program)

import Language.Boogie.Pretty                     (pretty)
import Language.Boogie.AST
import Language.Boogie.PrettyAST
import qualified Language.Boogie.Position as Pos
import System.IO.Unsafe                           (unsafePerformIO)

-- unsafe, oops
{-# NOINLINE checkProg #-}
checkProg :: Program -> Bool
checkProg p = unsafePerformIO $! compileProg "check.bpl" p >> checkProgFile "check.bpl" 

checkProgFile :: FilePath -> IO Bool
checkProgFile f = do {
  (_, _, _, ph) <- createProcess $ shell cmd;
  r <- waitForProcess ph;
  return $ r /= ExitSuccess
} where cmd     = prefix ++ " && ./boog.sh " ++ f ++ " | grep Error"
        prefix  = "echo \" CHECKING BOOGIE AT " ++ f ++ "\":"


debugProg :: FilePath -> Program -> Program
debugProg f p = unsafePerformIO $! compileProg f p >> return p

debugBoogie :: [Decl] -> [Decl]
debugBoogie p = unsafePerformIO $! compileProg "debug.bpl"  (Program p) >> return p

debugBlock :: Block -> Block
debugBlock b = unsafePerformIO $! compileProg "debug.bpl" prog >> return b
  where 
    prog = Program [Pos.gen $ ProcedureDecl "Main" [] [] [] [] $ Just ([], b)]
{-# NOINLINE compileProg #-}
compileProg :: String -> Program -> IO ()
compileProg path p = writeFile path (show $ pretty $ p)