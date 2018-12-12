{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}

module Language.Spyder.Translate (
    toBoogie
) where

import Language.Spyder.Translate.Direct

import Language.Spyder.AST                        (Program)
import Language.Spyder.AST.Imp
import Language.Spyder.AST.Spec
import Language.Spyder.AST.Component              (MainDecl(..), DerivDecl(..), Component(..))
import qualified Language.Boogie.AST as BST
import qualified Language.Boogie.Position as Pos
import Language.Spyder.Translate.Desugar
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List                                  (find, partition)
import Language.Spyder.Translate.Rename
import Language.Spyder.Synth.Verify               (checkProgFile)
import Language.Spyder.Synth                      (fixProc, fixProcGeneral)
import Language.Boogie.Pretty                     (pretty)
import System.IO.Unsafe                           (unsafePerformIO)

import Language.Spyder.Translate.Related

import Language.Spyder.Opt

import Control.Monad
import System.IO.Unsafe


toBoogie :: Program -> BST.Program
toBoogie prog@(comps, MainComp decls) = outProg 
  where
    
    (bprog, (invs, varMap, dims)) = translateProg prog
    boogDecs = case bprog of BST.Program ds -> map Pos.node ds
    
    reledVars = alphaRels (relatedVars prog) varMap
    -- attempt to gen the program. if the initial translated program verifies, then we're good to go. otherwise, 
    -- for each procedure that doesn't verify, synthesize a fix.
    boogHeader :: [BST.BareDecl]
    boogHeader = filter takeHeader boogDecs 

    funcs = filter takeProc boogDecs
    globals = filter takeGlobal boogDecs >>= getName

    (okProcs, brokenProcs) = partition (checkBoogie . linkHeader) funcs
    
    linkHeader :: BST.BareDecl -> BST.Program
    linkHeader pd = BST.Program $ map Pos.gen $ boogHeader ++ [pd]

    outProg = unsafePerformIO $ foldM repProc (BST.Program $ map Pos.gen $ boogHeader ++ okProcs) brokenProcs


    repProc :: BST.Program -> BST.BareDecl -> IO BST.Program
    repProc p (BST.ProcedureDecl nme tyargs formals rets contr (Just bod@(oldvars, fixme))) = do {
      putStrLn $ "COMPLETING: " ++ nme;
      let !ret = optimize $! BST.Program $! decs ++ [newProc] in do {
        putStrLn "Done!";
        return ret;
      }
    }
      where
        decs = case newProg of BST.Program i -> i
        dims' = dims `addITWs` oldvars
        repairTargetted = True
        newProc = Pos.gen $! BST.ProcedureDecl nme tyargs formals rets contr (Just (newvars, fixed))
        (fixed, newProg, (newvars, _)) = if repairTargetted 
          then fixProc dims' invs reledVars p bod globals fixme  
          else fixProcGeneral dims' invs p bod globals fixme
        
        
    takeHeader = not . takeProc -- everything but procedures
    takeProc x@BST.ProcedureDecl{} = True
    takeProc _ = False   -- just procedures
    takeGlobal x@BST.VarDecl{} = True
    takeGlobal _ = False -- just variables
    getName :: BST.BareDecl -> [String]
    getName (BST.VarDecl x) = map BST.itwId x





getRelNames :: Component -> [String]
getRelNames (DerivComp nme decs) = map worker relDecs
  where
    worker (RelDecl n _ _) = n
    worker _ = error "inconceivable"
    relDecs = filter takeRD decs
    takeRD RelDecl{} = True
    takeRD _ = False




checkBoogie :: BST.Program -> Bool
checkBoogie prog = unsafePerformIO $ compileProg >> checkProgFile outp
  where
    outp = "tmp.bpl"
    compileProg = writeFile outp (show $ pretty prog)