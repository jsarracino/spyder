{-# LANGUAGE LambdaCase #-}
module Language.Spyder.Synth.Cegis (
    repairBlock
  , boogExec
  , takeConsts
) where

import Language.Boogie.AST            
import Language.Boogie.Interpreter
import Language.Boogie.TypeChecker
import Language.Boogie.Solver
import Language.Boogie.Environment
import qualified Language.Boogie.Z3.Solver as Z3
import Language.Boogie.Generator

import qualified Language.Boogie.Position as Pos
import Language.Spyder.AST.Imp        (VDecl(..), stripTy)
import qualified Language.Spyder.Synth.Context as SC
import Language.Spyder.Synth.Enum     (RhsExpr(..), bases, translate, grow)
import Language.Spyder.Synth.Verify   (debugBoogie, checkProg)
import Language.Spyder.Translate.Direct
import qualified Data.Map.Strict as Map
import qualified Data.Set.Monad as Set
import Data.List                      (intersperse)
import Control.Monad.Logic
import Control.Monad.Stream
import Control.Lens hiding (Context, at)

get = (Map.!)
-- * assume there's one basic block, one variable to modify

-- initialize E := depth 1, IO := {}
-- 1: does it verify in boogie? if yes, done
  -- 2: otherwise, search through E as follows.
  -- 3: generate a control variable for selecting e \in E, and a switch statement assigning v := e.
  
  
  -- checkConfig(prog, config, E, IO) = run boogaloo test on (prog with config). 
  --   if it succeeds:
    --     verify with boogie. if that succeeds, return (prog, config)
      --     otherwise:
      --       let E' <- expand(E).
      --       let config <- searchConfig(prog, expressions, IO).
      --   if it fails with IO io:
        --     let IO' <- add io to IO. 
        --     let config <- searchConfig(prog, expressions, IO').
        --     return checkConfig(prog, config', E, IO').
        
        -- searchConfig(prog, expressions, IO) = 
          --   prog' <- prog.
          --   foreach io in IO:
          --     prog' <- specialize prog' to IO.
          --   run boogaloo exec on prog'. if it succeeds with config c, return c.
            --   otherwise, signal failure.
            
type Config = (String, Int) --Map.Map String Int
type IOExamples = [Map.Map String Value]
         
-- find a repair for a single lhs expr
-- given a set of invariants, a program preamble, global variables, a set of rhs expression seeds, a set of lhs expression seeds, and a basic block for repair, return a repair.
repairBlock :: [Expression] -> [Decl] -> [String] -> SC.Context -> VDecl -> Block -> Block
repairBlock invs prog globals rhsCtx lhsVar blk =  blk ++ fix
  where
    initConfig = (c, 0)
    c = "cegis__control" --allocateFreshVar prog (Just "cegis__control")
    prog' = prog ++ addControlVar c
    -- prog'' = debugBoogie prog'
    
    firstEx = case checkConfig invs prog' globals initConfig blk of 
      Left _ -> undefined "inconceivable"
      Right io -> io
    initIO = [firstEx]

    rhsExprs = bases rhsCtx
    finalExpr = searchAllConfigs invs prog' globals blk rhsCtx rhsExprs lhsVar initConfig initIO
    lhsAssgn = stmt $ Assign [(stripTy lhsVar, [])] [finalExpr]
    fix = [lhsAssgn]
    
-- given a set of invariants, a preamble, global variables, a block to fix, a set of base values, and a variable to add an edit, use cegis to
-- search for a configuration that correctly edits the variable. return the resulting block.

-- assumes at least one IO example
-- TODO: this logic
searchAllConfigs :: [Expression] -> [Decl] -> [String] -> Block -> SC.Context -> [RhsExpr] -> VDecl -> Config -> IOExamples -> Expression
searchAllConfigs invs prog globals blk ctx rhsEs lhs conf examples = result
  where
    (cname, configResult) = conf
    candExprs = map (Pos.gen . translateExpr) $ rhsEs >>= translate ctx
    procs = map buildProc $ [0..] `zip` examples
    -- ProcedureDecl Id [Id] [IdTypeWhere] [IdTypeWhere] [Contract] (Maybe Body)
    
    buildProc :: (Int, Map.Map String Value) -> Decl
    buildProc (n, io) = Pos.gen $ ProcedureDecl nme [] [] [] [Modifies False globals] (Just bod)
      where
        nme = makeName n
        bod = ([], addAssumes invs io withExprs)
    withExprs = linkExpressions candExprs cname (stripTy lhs) blk
    procNames = take (length examples) $ map makeName [0..]
    makeName n = "__spy__cegis" ++ show n

    loBound = Pos.gen $ BinaryExpression Leq (Pos.gen $ numeral 0) (Pos.gen $ Var cname)
    upBound = Pos.gen $ BinaryExpression Gt (Pos.gen $ numeral . fromIntegral $ length candExprs) (Pos.gen $ Var cname) 
    confAxioms = map (Pos.gen . AxiomDecl) [loBound, upBound]
  
    prog' = prog ++ confAxioms
    checkMe = Program $ prog' ++ procs ++ [buildMainSearch globals procNames]
    
    result = case boogExec checkMe "Main" of
      x:xs -> 
        let newConfig = (cname, asInt $ takeConsts x `get` cname) -- a new challenger appears. 
            newResult = checkConfig invs prog' globals newConfig withExprs in
        case newResult of 
          Left c    -> 
            if checkProg $ Program $ prog' ++ [buildConfVal c] ++ [buildMain invs globals withExprs] --it seems to work...use expensive check. if that fails, get more expressions?
            then candExprs !! snd c
            else searchAllConfigs invs prog' globals blk ctx (grow ctx rhsEs) lhs conf examples -- recurse
          Right io  -> searchAllConfigs invs prog' globals blk ctx rhsEs lhs newConfig (io:examples) -- recurse
      
      []   -> candExprs !! configResult --can't find a new candidate...O.O

      
buildConfVal :: Config -> Decl
buildConfVal (name, val) = Pos.gen $ AxiomDecl $ eq (Var name) (numeral $ toInteger val)
-- given a set of invariants, a preamble, a set of globals, and a block to check, either return the block (if it passes boogie test) or return more counterexamples
checkConfig :: [Expression] -> [Decl] -> [String] -> Config -> Block -> Either Config (Map.Map String Value)
checkConfig invs header globals config blk = result
  where
    prog = Program $ header ++ [main] ++ [buildConfVal config]
    
    main = buildMain invs globals blk 
    result = case boogTest prog "Main" of 
      []    -> Left config
      x:xs  -> Right $ buildIO x 
    verifies = True

    -- ProcedureDecl Id [Id] [IdTypeWhere] [IdTypeWhere] [Contract] (Maybe Body)
-- checkConfig(prog, config, E, IO) = run boogaloo test on (prog with config). 
  --   if it succeeds:
    --     verify with boogie. if that succeeds, return (prog, config)
      --     otherwise:
      --       let E' <- expand(E).
      --       let config <- searchConfig(prog, expressions, IO).
      --   if it fails with IO io:
        --     let IO' <- add io to IO. 
        --     let config <- searchConfig(prog, expressions, IO').
        --     return checkConfig(prog, config', E, IO').
-- evaluateConfig :: 


-- helper: given an IO and invs, add assumes to the begin/end of the block to specialize to the IO.
addAssumes :: [Expression] -> Map.Map String Value -> Block -> Block
addAssumes invs io block = map buildIO (Map.toList io) ++ block ++ map buildInv invs
  where
    --Predicate attrs (SpecClause _ isAssume e)
    buildInv :: Expression -> LStatement
    buildInv e = stmt $ Predicate [] (SpecClause Inline True e)
    buildIO :: (String, Value) -> LStatement
    buildIO ioPr = stmt $ Predicate [] (SpecClause Inline True $ buildExpr ioPr)
    buildExpr :: (String, Value) -> Expression
    buildExpr (l, r@IntValue{}) = eq (Var l) (Literal r)
    buildExpr (l, r@BoolValue{}) = eq (Var l) (Literal r)
    
eq :: BareExpression -> BareExpression -> Expression
eq l r = Pos.gen $ BinaryExpression Eq (Pos.gen l) (Pos.gen r)

buildMainSearch :: [String] -> [String] -> Decl
buildMainSearch globals functions = Pos.gen $ ProcedureDecl "Main" [] [] [] [Modifies False globals] (Just bod)
  where
    bod = ([], havoc `intersperse` map buildCalls functions)
    buildCalls fname = stmt $ Call [] fname []
    havoc = stmt $ Havoc globals

buildMain :: [Expression] -> [String] -> Block -> Decl
buildMain invs globals bod = Pos.gen $ ProcedureDecl "Main" [] [] [] (reqs ++ ensures ++ modifies) (Just ([], bod))
  where 
    reqs = map (Requires False) invs
    ensures = map (Ensures False) invs
    modifies = [Modifies False globals]

-- given a list of expressions, a control variable, and a lhs for assignment, add (at the end) in
-- a switch on the control var assigning each e to lhs
linkExpressions :: [Expression] -> String -> String -> Block -> Block
linkExpressions es control lhs blk = finalBlock
  where
    assgns = map (buildAssgn lhs) es
    finalBlock = blk ++ map buildIT (assgns `zip` [0..])

    buildAssgn l r = stmt $ Assign [(l, [])] [r]
    buildIT (a, cond) = stmt $ If (Expr $ eq (Var control) (Literal $ IntValue cond)) [a] Nothing


-- add the variable                           
addControlVar :: String -> [Decl]
addControlVar name = [Pos.gen vardec]
  where
    vardec = ConstantDecl True [name] IntType (Just []) True

stmt :: BareStatement -> LStatement
stmt s = Pos.gen ([], Pos.gen s)

boogExec :: Program -> String -> [TestCase]
boogExec p pname = case typeCheckProgram p of
              Left typeErrs -> undefined "inconceivable"
              Right context -> runInt context
  where 
    runInt ctx = take 1 $ filter keep . maybeTake exec_max $ int p ctx pname

    int = interpreter branch_max rec_max loop_max minimize concretize True

    branch_max = Just 128
    exec_max = Just 2048
    rec_max = Just 1024
    loop_max = Just 1000
    minimize = True
    concretize = True
    maybeTake = \case
      Nothing -> id
      Just n -> take n
    keep = not . isInvalid

boogTest :: Program -> String -> [TestCase]
boogTest p pname = case typeCheckProgram p of
              Left typeErrs -> undefined "inconceivable"
              Right context -> runInt context
  where 
    runInt ctx = take 1 $ filter keep . maybeTake exec_max $ int p ctx pname

    int = interpreter branch_max rec_max loop_max minimize concretize True

    branch_max = Just 128
    exec_max = Just 2048
    rec_max = Just 1024
    loop_max = Just 1000
    minimize = True
    concretize = True
    maybeTake = \case
      Nothing -> id
      Just n -> take n
    keep tc = (not . isInvalid) tc && (not . isPass) tc
      
takeConsts :: TestCase -> Map.Map String Value
takeConsts tc@(TestCase _ mem conMem _) = buildMap $ mem'^.memConstants
  where
    mem' :: Memory
    mem' = userMemory conMem mem
    buildMap :: Map.Map String Expression -> Map.Map String Value
    buildMap = Map.map (\case (Pos.Pos _ e) -> eval' e)


buildIO :: TestCase -> Map.Map String Value
buildIO tc@(TestCase _ mem conMem _) = buildMap $ mem'^.memOld -- we use memOld instead of memGlobals because memGlobals has the *failure state* -- we actually want the initial inputs
  where
    mem' :: Memory
    mem' = userMemory conMem mem
    buildMap :: Map.Map String Expression -> Map.Map String Value
    buildMap = Map.map (\case (Pos.Pos _ e) -> eval' e)
-- I could just use eval, but I'm not sure how the monads work, and we shouldn't
-- get any free variables in these expressions anyway (because it's a thunk)
eval' :: BareExpression -> Value
eval' (Literal v) = v
eval' _ = undefined "TODO"

unvalue :: Value -> Expression
unvalue v@IntValue{} = Pos.gen $ Literal v
unvalue _ = undefined "TODO"

asInt :: Value -> Int 
asInt (IntValue v) = fromIntegral v
asInt _ = undefined "inconceivable"

--       -- | Default backtracking strategy
-- defaultBT = DF

-- -- | Default solutions per path
-- defaultPerPath = 128

-- -- | Default number of test cases for testing
-- defaultTCCount = 2048

-- -- | Default maximum number of loop iterations
-- defaultLMax = 1000
  --     execute = Exec {
  -- proc_       = Nothing         &= help "Program entry point (default: Main or the only procedure in a file)" &= typ "PROCEDURE" &= name "p",
  -- file        = ""              &= typFile &= argPos 0,
  -- -- backtrack   = defaultBT       &= help ("Backtracking strategy: DF (depth-first) or Fair (default: " ++ show defaultBT ++ ")"),
  -- per_path    = defaultPerPath  &= help ("Maximum number of solutions to try per path; " ++
  --                                       "unbounded if negative (default: " ++ show defaultPerPath ++ ")") &= name "n",
  -- exec        = -1              &= help ("Maximum number of executions to try; unbounded if negative (default: -1)"),
  -- rec_max     = -1              &= help ("Maximum number of unfolds for a recursive constraint; unbounded if negative (default: -1)"),
  -- loop_max    = defaultLMax     &= help ("Maximum number of loop iterations; unbounded if negative (default: " ++ show defaultLMax ++ ")"),
  -- minimize    = True            &= help ("Should solutions be minimized? (default: True)"),
  -- concretize  = True            &= help ("Concretize in the middle of an execution? (default: True)"),
  -- invalid     = False           &= help "Display invalid executions (default: false)" &= name "I",
  -- nexec       = True            &= help "Display executions that cannot be carried out completely (default: true)" &= name "N",
  -- pass        = True            &= help "Display passing executions (default: true)" &= name "P",
  -- fail_       = True            &= help "Display failing executions (default: true)" &= name "F",
  -- out         = 1               &= help "Maximum number of executions to display; unbounded if negative (default: 1)",
  -- summary_    = False           &= help "Only print a summary of all executions and a list of unique failures (default: false)" &= name "S",
  -- debug       = False           &= help "Debug output (default: false)",
  -- format      = defaultFormat   &= help ("Output format: Plain, Ansi or Html (default: " ++ show defaultFormat ++ ")")
  -- } &= auto &= help "Execute program"

interpreter :: Maybe Int -> Maybe Int ->  Maybe Int -> Bool -> Bool -> Bool -> (Program -> Context -> Id -> [TestCase])
interpreter branch_max rec_max loop_max minimize concretize solvePassing p context proc_ = toList $ executeProgram p context solver rec_max loop_max concretize solvePassing generator proc_
  where
    solver :: Solver Logic
    solver = Z3.solver minimize branch_max
    generator = exhaustiveGenerator Nothing