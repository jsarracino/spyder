module Language.Spyder.Synth (
  --   generateProgs
  -- , buildContext
  -- , 
    fixProc
  , fixBlock
  , fixStmt
) where

import Language.Spyder.Synth.Verify
import Language.Spyder.Synth.Enum
import Language.Spyder.Synth.Cegis            (repairBlock, buildMain)
import qualified Language.Spyder.AST as SAST                   
import Language.Spyder.AST.Component          (Component(..), MainDecl(..))
import Language.Spyder.AST.Imp                (VDecl, Type(..))
import Language.Spyder.Synth.Context          (buildContext)
import Language.Boogie.AST                    
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Language.Boogie.Position               (node, Pos(..), gen)
import Data.List                              (delete, nub, intersect, (\\))
import Language.Spyder.Opt 

import Language.Spyder.Translate.Related

-- given a program and a basic block *to fix*, run cegis to search for the fix.
fixProc :: [Expression] -> [Set.Set String] -> Program -> Body -> [String] -> SAST.Program -> Block -> (Block, Program, Body)
fixProc invs relVars header body globals p@(comps, MainComp decs) broken = fixed
  where
    fixed = fixBlock invs relVars header globals rhsVars body [] broken
    rhsVars = findInScope header body invs

fixBlock :: [Expression] -> [Set.Set String] -> Program -> [String] -> [String] -> Body -> Block -> Block -> (Block, Program, Body)
fixBlock invs relVars header globals rhsVars scope prefix fixme = fixResult inner
  where
    inner = foldl wrapFixStmt init fixme
    fixResult :: (Block, Program, Body) -> (Block, Program, Body)
    fixResult x@(blk, prog@(Program decs), scope'@(vs, _)) = if not isRepaired then fixed else x
      where
        isRepaired = checkProg $ optimize $ Program $ decs ++ [buildMain invs globals (vs, blk)]
        fixed = repairBlock invs prog globals (findUnedited relVars rhsVars blk) rhsVars scope' blk

    init = (prefix, header, scope)
    wrapFixStmt :: (Block, Program, Body) -> LStatement -> (Block, Program, Body)
    wrapFixStmt (blk, prog, bod) (Pos o (ls, (Pos i s))) = (blk', prog', bod')
      where
        (s', prog', bod') = fixStmt invs relVars globals rhsVars (blk, prog, bod) s
        blk' = blk ++ [Pos o (ls, (Pos i s'))]

    -- repairBlock :: [Expression] -> Program -> [String] -> [String] -> [String] -> Body -> Block -> (Block, Program, Body)


fixStmt :: [Expression] -> [Set.Set String] -> [String] -> [String] -> (Block, Program, Body) -> BareStatement -> (BareStatement, Program, Body)
fixStmt invs relVars globals rhsVars (prefix, prog, scope) = worker
   where
    worker (If e tru fls) = (If e tru' fls', prog'', bod'')
      where
        (tru', prog', bod') = recur prog scope tru
        (fls', prog'', bod'') = case fls of 
          Just i  -> let (r, p, b) = recur prog' bod' i in (Just r, p, b)
          Nothing -> (Nothing, prog', bod')
    worker s = (s, prog, scope)
    recur p b s = fixBlock invs relVars p globals rhsVars b prefix s

findEdited :: Block -> [String]
findEdited = foldl recurLS [] -- blk
  where
    recurLS :: [String] -> LStatement -> [String]
    recurLS edits ss = recurBLS edits $ node ss
    recurBLS :: [String] -> BareLStatement -> [String]
    recurBLS edits (_, stmt) = recurStmt edits stmt
    recurStmt :: [String] -> Statement -> [String]
    recurStmt edits s = recurBStmt edits (node s)
    recurBStmt edits (Assign assns _) = map fst assns
    recurBStmt edits _ = edits

findUnedited :: [Set.Set String] -> [String] -> Block -> [String]
findUnedited rels globals blk = (computeRels (foldl recurLS globals blk) rels) \\ used
  where
    used = findEdited blk
    recurLS :: [String] -> LStatement -> [String]
    recurLS edits ss = recurBLS edits $ node ss
    recurBLS :: [String] -> BareLStatement -> [String]
    recurBLS edits (_, stmt) = recurStmt edits stmt
    recurStmt :: [String] -> Statement -> [String]
    recurStmt edits s = recurBStmt edits (node s)
    recurBStmt edits (Assign assns _) = foldl (flip delete) edits $ map fst assns
    recurBStmt edits _ = edits

findInScope :: Program -> Body -> [Expression] -> [String]
findInScope (Program decs) _ desugInvs = (decs >>= (getVars . node)) `intersect` invVars
  where
    getVars (VarDecl itws) = map itwId itws
    getVars _ = []
    invVars = gatherVars desugInvs

gatherVars :: [Expression] -> [String]
gatherVars es = nub $ es >>= recurE
  where 
    recurE :: Expression -> [String]
    recurE = recur . node
    recur :: BareExpression -> [String]
    recur (Var x) = [x]
    recur (Application _ es) = nub $ es >>= recurE
    recur (BinaryExpression _ l r) = recurE l ++ recurE r
    recur (UnaryExpression _ i) = recurE i
    recur x@Literal{} = []
    -- MapSel, Quantified, MapUp TODO
            