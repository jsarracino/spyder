module Language.Spyder.Synth (
  --   generateProgs
  -- , buildContext
  -- , 
  fixProc
) where

import Language.Spyder.Synth.Verify
import Language.Spyder.Synth.Enum
import Language.Spyder.Synth.Cegis            (repairBlock)
import qualified Language.Spyder.AST as SAST                   
import Language.Spyder.AST.Component          (Component(..), MainDecl(..))
import Language.Spyder.AST.Imp                (VDecl, Type(..))
import Language.Spyder.Synth.Context          (buildContext)
import Language.Boogie.AST                    
import qualified Data.Map.Strict as Map
import Language.Boogie.Position               (node)
import Data.List                              (delete)


-- generateProgs :: Program -> [Program]
-- buildContext :: Program -> Context

-- given a program and a basic block *to fix*, run cegis to search for the fix.
fixProc :: [Expression] -> Program -> Body -> [String] -> SAST.Program -> Map.Map String String -> Block -> (Block, Program, Body)
fixProc invs header body globals p@(comps, MainComp decs) varmap broken = fixed
  where
    fixed = repairBlock invs header globals lhsVars rhsVars body broken
    lhsVars = findUnedited rhsVars broken 
    rhsVars = findInScope header body
    rhsContext = buildContext p varmap

-- TODO: soundly handle labels (?)
findUnedited :: [String] -> Block -> [String]
findUnedited = foldl recurLS
  where
    recurLS :: [String] -> LStatement -> [String]
    recurLS edits ss = recurBLS edits $ node ss
    recurBLS :: [String] -> BareLStatement -> [String]
    recurBLS edits (_, stmt) = recurStmt edits stmt
    recurStmt :: [String] -> Statement -> [String]
    recurStmt edits s = recurBStmt edits (node s)
    recurBStmt edits (Assign assns _) = foldl (flip delete) edits $ map fst assns
    recurBStmt edits (If _ t f) = final
      where
        truUnedited = findUnedited edits t
        final = case f of 
          Just b -> findUnedited truUnedited b
          _      -> truUnedited
    recurBStmt edits (While _ _ b) = findUnedited edits b
    recurBStmt edits _ = edits

findInScope :: Program -> Body -> [String]
findInScope (Program decs) _ = decs >>= (getVars . node)
  where
    getVars (VarDecl itws) = map itwId itws
    getVars _ = []

            