{-# LANGUAGE LambdaCase #-}
module Language.Spyder.Synth.Cegis (
    repairBlock
  , boogExec
  , takeConsts
  , checkConfig
  , boogTest
  , buildConfVal
  , buildMain
  , generateFix
  , allocFreshConst
  , generateSwitch
  , buildBounds
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
import Language.Spyder.Synth.Verify   (debugBoogie, checkProg, debugBlock, debugProg)
import Language.Spyder.Translate.Direct
import qualified Data.Map.Strict as Map
import qualified Data.Set.Monad as Set
import Data.List                      (intersperse, foldl')
import Control.Monad.Logic
import Control.Monad.Stream
import Control.Lens hiding (Context, at)
import Language.Spyder.Util                       (stripPos)

-- get = (Map.!)

-- assumes the const is an int
allocFreshConst :: Program -> (String, Program)
allocFreshConst (Program decs) = (name ++ show suffix, Program $ vdec:decs)
  where
    name = "__cegisvar__"
    suffix = worker 0 $ Set.fromList $ (map stripPos decs) >>= getNames
    vdec = Pos.gen $ ConstantDecl True [name ++ show suffix] IntType (Just []) True

    getNames :: BareDecl -> [String]
    getNames (ConstantDecl _ xs _ _ _) = xs
    getNames (VarDecl itws ) = map itwId itws
    getNames _ = []
    -- check name clashes with other variables and constants
    worker :: Int -> Set.Set String -> Int
    worker suf names = if (name ++ show suf) `Set.member` names then worker (suf+1) names else suf

-- given a list of rhs expressions and a lhs assignment, generate a switch for lhs := rhs_0 | rhs_1 | ... | rhs_n. return the name of the control variable
-- for choosing the expression, as well as the switch in block form.
generateSwitch :: [Expression] -> String -> Program -> (String, Block, Program)
generateSwitch es lhs prog = (controlName, finalBlock, finalProg)
  where
    (controlName, finalProg) = allocFreshConst prog

    assgns = map (buildAssgn lhs) es
    finalBlock = zipWith buildIT assgns [0..]

    buildAssgn l r = stmt $ Assign [(l, [])] [r]
    buildIT a cond = stmt $ If (Expr $ eq (Var controlName) (Literal $ IntValue cond)) [a] Nothing
   
-- given a depth, candidate variables, lhs for assign, program, and block to fix, generate a fix involving assignments to lhs using the candidates.
-- also add in an axiom for the size of the switch variable.
generateFix :: Int -> [String] -> String -> Program -> Block -> (Program, Block)
-- at depth 0, allocate a constant variable, and choose between the candidates and the constant.
generateFix 0 cands lhs prog fixme = (finalProg, fixme ++ newBlock)
  where
    (constVar, withConst) = allocFreshConst prog
    rhsEs = map (Pos.gen . Var) (constVar:cands)
    (switchVar, newBlock, withSwitch) = generateSwitch rhsEs lhs withConst
    finalProg = addBounds withSwitch
    addBounds (Program decs) = Program $ bounds ++ decs
      where bounds = map (Pos.gen . AxiomDecl) (buildBounds (length rhsEs) switchVar)

-- at depth n, allocate two variables for n-1 depths, and build binops/unops from the smaller vars
generateFix n cands lhs prog fixme = (finalProg, rBlock ++ newBlock)
  where
    (lvar, withL) = allocFreshConst prog
    (rvar, withR) = allocFreshConst withL

    (recurL, lBlock) = generateFix (n-1) cands lvar withR fixme
    (recurR, rBlock) = generateFix (n-1) cands rvar recurL lBlock
    
    rhsEs = lv:binops ++ unops

    (switchVar, newBlock, withSwitch) = generateSwitch rhsEs lhs recurR
    finalProg = addBounds withSwitch
    addBounds (Program decs) = Program $ bounds ++ decs
      where 
        bounds = map (Pos.gen . AxiomDecl) (buildBounds (length rhsEs) switchVar)
    
    lv = Pos.gen $ Var lvar
    rv = Pos.gen $ Var rvar
    binops = [Pos.gen $ BinaryExpression op lv rv | op <- [Plus, Minus, Times]]
    unops = [Pos.gen $ UnaryExpression Neg lv]
buildBounds :: Int -> String -> [Expression]
buildBounds upper name = [lo, hi]
  where 
    v = Pos.gen $ Var name
    lo = Pos.gen $ BinaryExpression Leq (Pos.gen $ numeral 0) v
    hi = Pos.gen $ BinaryExpression Gt (Pos.gen $ numeral $ fromIntegral upper) v
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
            
type Config = Map.Map String Int
type IOExamples = [Map.Map String Value]
         
-- find a repair for a single lhs expr
-- given a set of invariants, a program preamble, global variables, a set of rhs expression seeds, a set of lhs expression seeds, and a basic block for repair, return a repair.
repairBlock :: [Expression] -> [Decl] -> [String] -> SC.Context -> VDecl -> Block -> Block
-- repairBlock invs prog globals rhsCtx lhsVar blk =  blk ++ fix
--   where
--     initConfig = Map.fromList [(c, 0)]
--     c = "cegis__control" --allocateFreshVar prog (Just "cegis__control")
--     prog' = prog ++ addCegisVars initConfig
    
--     firstEx = case checkConfig invs prog' globals initConfig blk of 
--       Left _ -> undefined "inconceivable"
--       Right io -> io
--     initIO = [firstEx]

--     rhsExprs = bases rhsCtx
--     finalExpr = searchAllConfigs invs prog' globals blk rhsCtx rhsExprs lhsVar initConfig initIO
--     lhsAssgn = stmt $ Assign [(stripTy lhsVar, [])] [finalExpr]
--     fix = [lhsAssgn]
repairBlock = undefined "TODO"
    
-- given a set of invariants, a preamble, global variables, a block to fix, a set of base values, and a variable to add an edit, use cegis to
-- search for a configuration that correctly edits the variable. return the resulting block.

-- assumes at least one IO example
searchAllConfigs :: [Expression] -> [Decl] -> [String] -> Block -> SC.Context -> [RhsExpr] -> VDecl -> Config -> IOExamples -> Expression
-- searchAllConfigs invs prog globals blk ctx rhsEs lhs conf examples = result
--   where
--     -- (cname, configResult) = conf
--     candExprs = map (Pos.gen . translateExpr) $ rhsEs >>= translate ctx
--     procs = map buildProc $ [0..] `zip` examples
    
--     buildProc :: (Int, Map.Map String Value) -> Decl
--     buildProc (n, io) = Pos.gen $ ProcedureDecl nme [] [] [] [Modifies False globals] (Just bod)
--       where
--         nme = makeName n
--         bod = ([], addAssumes invs io withExprs)
--     withExprs = linkExpressions candExprs conf (stripTy lhs) blk
--     procNames = take (length examples) $ map makeName [0..]
--     makeName n = "__spy__cegis" ++ show n

--     bounds = buildBounds conf "cegis__control"
    
--     confAxioms = map (Pos.gen . AxiomDecl) bounds
  
--     prog' = prog ++ confAxioms
--     checkMe = Program $ prog' ++ procs ++ [buildMainSearch globals procNames]
    
--     result = case boogExec checkMe "Main" of
--       x:xs -> 
--         let newConfig = Map.map asInt $ takeConsts x-- a new challenger appears
--             newResult = checkConfig invs prog' globals newConfig withExprs in
--         case newResult of 
--           Left c    -> 
--             if checkProg $ Program $ prog' ++ [buildConfVal c] ++ [buildMain invs globals withExprs] --the candidate seems to work...use expensive check. if that fails, get more expressions
--             then candExprs !! snd c  -- we have a winner!
--             else searchAllConfigs invs prog' globals blk ctx (grow ctx rhsEs) lhs conf examples -- get more exprs
--           Right io  -> searchAllConfigs invs prog globals blk ctx rhsEs lhs newConfig (io:examples) -- retry current exprs
      
--       []   -> searchAllConfigs invs prog globals blk ctx (grow ctx rhsEs) lhs conf examples -- can't find a new candidate...O.O
searchAllConfigs = undefined "TODO"


      
buildConfVal :: Config -> Decl
-- buildConfVal (name, val) = Pos.gen $ AxiomDecl $ eq (Var name) (numeral $ toInteger val)
buildConfVal = undefined "TODO"
-- given a set of invariants, a preamble, a set of globals, and a block to check, either return the block (if it passes boogie test) or return more counterexamples
checkConfig :: [Expression] -> [Decl] -> [String] -> Config -> Block -> Either Config (Map.Map String Value)
-- checkConfig invs header globals config blk = result
--   where
--     prog =  Program $ header ++ [main] ++ [buildConfVal config]
    
--     main = buildMain invs globals blk 
--     result = case boogTest prog "Main" of 
--       []    -> Left config
--       x:xs  -> Right $ buildIO x 
--     verifies = True
checkConfig = undefined "TODO"

-- given invs and a set of io, add assumes to the begin/end of the block to specialize to the IO.
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

-- build main function for exec phase
buildMainSearch :: [String] -> [String] -> Decl
buildMainSearch globals functions = Pos.gen $ ProcedureDecl "Main" [] [] [] [Modifies False globals] (Just bod)
  where
    bod = ([], havoc `intersperse` map buildCalls functions)
    buildCalls fname = stmt $ Call [] fname []
    havoc = stmt $ Havoc globals

-- build main function for test/verify phases
buildMain :: [Expression] -> [String] -> Block -> Decl
buildMain invs globals bod = Pos.gen $ ProcedureDecl "Main" [] [] [] (reqs ++ ensures ++ modifies) (Just ([], bod))
  where 
    reqs = map (Requires False) invs
    ensures = map (Ensures False) invs
    modifies = [Modifies False globals]

-- given a list of expressions, a control variable, and a lhs for assignment, add (at the end)
-- a switch on the control var assigning each e to lhs
-- linkExpressions :: [Expression] -> Config -> String -> Block -> Block
-- linkExpressions es conf lhs blk = finalBlock
--   where
--     assgns = map (buildAssgn lhs) es
--     finalBlock = blk ++ map buildIT (assgns `zip` [0..])

--     buildAssgn l r = stmt $ Assign [(l, [])] [r]
--     buildIT (a, cond) = stmt $ If (Expr $ eq (Var control) (Literal $ IntValue cond)) [a] Nothing


-- add a control var decl                           
addCegisVars :: Config -> [Decl]
-- addCegisVars name = [Pos.gen vardec]
--   where
--     vardec = ConstantDecl True [name] IntType (Just []) True
addCegisVars = undefined "TODO"

stmt :: BareStatement -> LStatement
stmt s = Pos.gen ([], Pos.gen s)

-- run in exec mode, searching for a valid configuration.
boogExec :: Program -> String -> [TestCase]
boogExec p pname = case typeCheckProgram p of
              Left typeErrs -> undefined "inconceivable"
              Right context -> runInt context
  where 
    runInt ctx = take 1 $ filter keep . maybeTake exec_max $ int p ctx pname

    int = interpreter branch_max rec_max loop_max minimize concretize True

    branch_max = Nothing--Just 
    exec_max = Nothing -- 2048
    rec_max = Nothing -- Just (-1)
    loop_max = Nothing --Just (-1)
    minimize = True
    concretize = True
    maybeTake = \case
      Nothing -> id
      Just n -> take n
    keep tc = (not . isInvalid) tc && (not . isNonexecutable) tc && (not . isFail) tc

-- run in test mode, searching for an invalid configuration.
boogTest :: Program -> String -> [TestCase]
boogTest p pname = case typeCheckProgram p of
              Left typeErrs -> undefined "inconceivable"
              Right context -> runInt context
  where 
    runInt ctx = take 1 $ filter keep . maybeTake exec_max $ int p ctx pname

    int = interpreter branch_max rec_max loop_max minimize concretize True

    branch_max = Just 128
    exec_max = Just 2048
    rec_max = Nothing -- Just (-1)
    loop_max = Nothing -- (-1)
    minimize = True
    concretize = True
    maybeTake = \case
      Nothing -> id
      Just n -> take n
    keep tc = (not . isInvalid) tc && (not . isPass) tc && (not . isNonexecutable) tc
      
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