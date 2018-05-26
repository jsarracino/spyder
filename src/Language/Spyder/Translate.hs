{-# LANGUAGE LambdaCase #-}

module Language.Spyder.Translate (
    toBoogie
  , translateExpr
  , translateStmt
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
import Language.Spyder.Translate.Derived          (instantiate, prefixApps)
import Data.List                                  (find, partition)
import Language.Spyder.Translate.Rename
import Language.Spyder.Synth.Verify               (checkProgFile)
import Language.Spyder.Synth                      (fixProc)
import Language.Boogie.Pretty                     (pretty)
import System.IO.Unsafe                           (unsafePerformIO)

import Language.Spyder.Translate.Related

import Language.Spyder.Opt


toBoogie :: Program -> BST.Program
toBoogie prog@(comps, MainComp decls) = outProg 
  where
    
    (bprog, (invs, varMap)) = translateProg prog
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

    outProg = foldl repProc (BST.Program $ map Pos.gen $ boogHeader ++ okProcs) brokenProcs

    repProc :: BST.Program -> BST.BareDecl -> BST.Program
    repProc p (BST.ProcedureDecl nme tyargs formals rets contr (Just bod@(oldvars, fixme))) = optimize $ BST.Program $ decs ++ [newProc]
      where
        decs = case newProg of BST.Program i -> i
        newProc = Pos.gen $ BST.ProcedureDecl nme tyargs formals rets contr (Just (newvars, fixed))
        (fixed, newProg, (newvars, _)) = fixProc invs reledVars p bod globals prog fixme  
        
        
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
    worker _ = undefined "inconceivable"
    relDecs = filter takeRD decs
    takeRD RelDecl{} = True
    takeRD _ = False




checkBoogie :: BST.Program -> Bool
checkBoogie prog = unsafePerformIO $ compileProg >> checkProgFile outp
  where
    outp = "tmp.bpl"
    compileProg = writeFile outp (show $ pretty prog)