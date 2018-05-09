module Language.Spyder.Synth (
  --   generateProgs
  -- , buildContext
  -- , 
  fixProc
) where

import Language.Spyder.Synth.Verify
import Language.Spyder.Synth.Enum
import Language.Spyder.Synth.Cegis            (repairBlock)
import Language.Spyder.AST                    (Program(..))
import Language.Spyder.AST.Component          (Component(..), MainDecl(..))
import Language.Spyder.AST.Imp                (VDecl, Type(..))
import Language.Spyder.Synth.Context          (buildContext)
import Language.Boogie.AST                    (Block, Decl, Expression)
import qualified Data.Map.Strict as Map


-- generateProgs :: Program -> [Program]
-- buildContext :: Program -> Context

-- given a program and a basic block *to fix*, run cegis to search for the fix.
fixProc :: [Expression] -> [Decl] -> [String] -> Program -> Map.Map String String -> Block -> Block
fixProc invs header globals p@(comps, MainComp decs) varmap broken = fixed
  where
    fixed = repairBlock invs header globals rhsContext lhsVar broken
    lhsVar = findUnedited globals broken -- ugh
    rhsContext = buildContext p varmap

-- TODO
findUnedited :: [String] -> Block -> VDecl
findUnedited _ _ = ("Main$y", BaseTy "int") 

            