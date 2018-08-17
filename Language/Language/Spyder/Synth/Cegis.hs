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
  , buildIO
  , buildMainSearch
) where

import Language.Boogie.AST            
import Language.Boogie.Interpreter
import Language.Spyder.Synth.Template 
import Language.Boogie.TypeChecker
import Language.Boogie.Solver
import Language.Boogie.Environment
import qualified Language.Boogie.Z3.Solver as Z3
import Language.Boogie.Generator

import Language.Spyder.Translate.Expr

import qualified Language.Boogie.Position as Pos
import Language.Spyder.AST.Imp        (VDecl(..), stripTy)
import qualified Language.Spyder.Synth.Context as SC
import Language.Spyder.Synth.Enum     (RhsExpr(..), bases, translate, grow)
import Language.Spyder.Synth.Verify   (debugBoogie, checkProg, debugBlock, debugProg)
import Language.Spyder.Translate.Direct
import qualified Data.Map.Strict as Map
import qualified Data.Set.Monad as Set
import Data.List                      (intersperse, foldl', nub, sortBy)
import Control.Monad
import Control.Monad.Logic
import Control.Monad.Stream
import Control.Lens hiding (Context, at)
import Language.Spyder.Util                       (allocFreshLocal, front, stmt)
import Data.Ord                                   (comparing)

import Language.Spyder.Config                     (concretSize)
import Language.Spyder.Opt


-- get = (Map.!)


-- assumes the const is an int
-- params prefix, where to allocate, variable type, program (to check for global clashes)
-- returns (name of var, new program (with var))
allocFreshConst :: Program -> (String, Program)
allocFreshConst (Program decs) = (name ++ show suffix, Program $ vdec:decs)
  where
    name = "__cegis__const"
    suffix = worker 0 $ Set.fromList $ map Pos.node decs >>= getNames
    vdec = Pos.gen $ ConstantDecl False [name ++ show suffix] IntType (Just []) True

    getNames :: BareDecl -> [String]
    getNames (ConstantDecl _ xs _ _ _) = xs
    getNames (VarDecl itws ) = map itwId itws
    getNames _ = []
    -- check name clashes with other variables and constants
    worker :: Int -> Set.Set String -> Int
    worker suf names = if (name ++ show suf) `Set.member` names then worker (suf+1) names else suf

-- TODO: refactor and eliminate Program
allocCegisLocal :: Program -> Body -> (String, Body, Program)
allocCegisLocal prog (itws, blk)  = (var, ([itws'], blk), prog)
  where
    (var, itws') = allocFreshLocal "__cegis__local" IntType (join itws)


buildSwitch :: String -> [Expression] -> Program -> (String, Block, Program)
buildSwitch lhs es prog = (switchVar, finalBlock, finalProg)
  where
    (switchVar, finalBlock, prog') = generateSwitch es lhs prog
    finalProg = case prog' of 
      (Program decs) -> Program $ decs ++ (map (Pos.gen . AxiomDecl) $ buildBounds 0 (length es) switchVar)

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
   
-- given a depth, candidate variables, lhs for assign, program, function scope, and block to fix, generate a fix involving assignments to lhs using the candidates.
-- also add in an axiom for the size of the switch variable.
-- TODO: refactor to use state monad for variable bookkeeping
generateFix :: Int -> [String] -> String -> Program -> Body -> (Program, Body, Block)
-- at depth 0, allocate a constant variable, and choose between the candidates and the constant.
generateFix 0 cands lhs prog scope = (finalProg, scope, newBlock)
  where
    (constVar, withConst) = allocFreshConst prog
    rhsEs = map (Pos.gen . Var) (constVar:cands)
    (switchVar, newBlock, finalProg) = buildSwitch lhs rhsEs withConst
-- TODO: error in num-cond2.spy with depth=1 (i think)

-- at depth n, allocate two variables for n-1 depths, and build binops/unops from the smaller vars
generateFix n cands lhs prog scope = (finalProg, newScope, lBlock ++ rBlock ++ newBlock)
  where
    (lvar, scope', prog') = allocCegisLocal prog scope
    (rvar, scope'', prog'') = allocCegisLocal prog' scope'

    (recurL, scope_, lBlock) = generateFix 0 cands lvar prog'' scope''
    (recurR, scopeR, rBlock) = generateFix (n-1) cands rvar recurL scope_
    
    rhsEs = [lv, rv] ++ binops ++ unops

    (resVar, newScope, resProg) = allocCegisLocal recurR scopeR

    (switchConst, withRes, withSwitch) = generateSwitch rhsEs resVar resProg
    newBlock = withRes ++ [stmt $ Assign [(lhs, [])] [Pos.gen $ Var resVar]]
    finalProg = addBounds withSwitch

    addBounds (Program decs) = Program $ bounds ++ decs
      where 
        bounds = map (Pos.gen . AxiomDecl) (buildBounds 0 (length rhsEs) switchConst)
    
    lv = Pos.gen $ Var lvar
    rv = Pos.gen $ Var rvar
    binops = [Pos.gen $ BinaryExpression op lv rv | op <- [Plus, Minus, Times]]
    unops = [Pos.gen $ UnaryExpression Neg rv]
    
buildBounds :: Int -> Int -> String -> [Expression]
buildBounds lower upper name = [lo, hi]
  where 
    v = Pos.gen $ Var name
    lo = Pos.gen $ BinaryExpression Leq (Pos.gen $ numeral $ fromIntegral lower) v
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
            
type Candidate = Map.Map String Int -- lhs depths
type Config = Map.Map String Int    -- control var to value
type IOExamples = [Map.Map String Value] -- list of states, where each state is a map from (global) variables to values
         
-- find a repair for a single lhs expr
-- params: invariants, program preamble, global variables, rhs expression seeds, lhs expressions to fix, enclosing function scope, and a basic block for repair.
-- returns: the repaired block, a new program, and a new function scope
repairBlock :: [Expression] -> Program -> [String] -> [String] -> [String] -> Body -> Block -> Block -> (Block, Program, Body)
repairBlock invs prog globals lhsVars rhsVars scope blk templ = ret
  where
    initConfig = Map.fromList []
    ret =  case checkConfig invs prog globals initConfig scope blk of 
      Left _ -> (blk, prog, scope)
      Right io -> let (finalProg, newScope, fixedBlock) = searchAllConfigs invs prog globals scope blk templ lhsVars rhsVars initCandDepths initConfig [io] in
        (fixedBlock, optimize finalProg, newScope)
        
    initCandDepths = [Map.fromList $ lhsVars `zip` repeat 0]

-- given a set of invariants, a preamble, global variables, a block to fix, a set of base values, and a variable to add an edit, use cegis to
-- search for a configuration that correctly edits the variable. return a map from the lhs values to their RHS blocks.

-- assumes at least one IO example
searchAllConfigs :: [Expression] -> Program -> [String] -> Body -> Block -> Block -> [String] -> [String] -> [Candidate] -> Config -> IOExamples -> (Program, Body, Block)
searchAllConfigs invs prog globals scope blk fillme lhses rhses (cand:cands) conf examples = result
  where
    -- (cname, configResult) = conf
    -- foreach lhs -> depth, look for a fix using depth. link all together.
    (searchProg, searchScope, newFixBlock) = Map.foldlWithKey genFix (prog, scope, fillme) cand
    -- newFixBlock = fillHoles newFixMap templ

    genFix :: (Program, Body, Block) -> String -> Int -> (Program, Body, Block)
    genFix (p, scop, acc) lhs depth = 
      let (p', s', nxt) = generateFix depth rhses lhs p scop in
        (p', s', substInHole True acc lhs nxt)

    searchBlock = blk ++ newFixBlock

    searchBody = case searchScope of (vars, _) -> (vars, searchBlock)
        

    procs = map buildProc $ [0..] `zip` examples
    
    buildProc :: (Int, Map.Map String Value) -> Decl
    buildProc (n, io) = Pos.gen $ ProcedureDecl nme [] [] [] [Modifies False globals] (Just bod)
      where
        nme = makeName n
        (varNames, _) = searchScope
        bod = (varNames, addAssumes invs io searchBlock)
    procNames = take (length examples) $ map makeName [0..]
    makeName n = "__cegis__func" ++ show n

    newCands = sortBy (comparing maxVal) $ map buildCand (Map.toList cand)
    buildCand (c, v) = Map.insert c (v+1) cand
    maxVal mp = maximum $ Map.elems mp 
    
    -- debugProg "cegis-search-debug.bpl" $!
  
    checkMe = optimize $ buildMainSearch globals procNames $ case searchProg of Program x -> Program $ x ++ procs

    buildAns :: Config -> (Program, Body, Block)
    buildAns cs = (finalProg, searchBody, newFixBlock )
      where
        synthDecs = case searchProg of Program decs -> decs
        finalProg = Program $ synthDecs ++ buildConfVal cs
    
    result = case boogExec checkMe "Main" of
      x:xs -> 
        let newConfig = Map.map asInt $ takeConsts x-- a new challenger appears
            newResult = checkConfig invs searchProg globals newConfig searchBody searchBlock in
        case newResult of 
          Left c    -> 
            if checkProg $ case searchProg of (Program decs) -> optimize $ Program $ decs ++ buildConfVal c ++ [buildMain invs globals searchBody] --the candidate seems to work...use expensive check. if that fails, get more expressions
            then buildAns c  -- we have a winner!
            else searchAllConfigs invs prog globals scope blk fillme lhses rhses (cands ++ newCands) newConfig examples -- get more exprs
          Right io  -> searchAllConfigs invs prog globals scope blk fillme lhses rhses (cand:cands) newConfig (io:examples) -- retry current exprs
      
      []   -> searchAllConfigs invs prog globals scope blk fillme lhses rhses (cands ++ newCands) conf examples -- can't find a new candidate...O.O



      
buildConfVal :: Config -> [Decl]
buildConfVal cs = map buildAsn $ Map.toList cs 
  where buildAsn (name, val) = Pos.gen $ AxiomDecl $ eq (Var name) (numeral $ toInteger val)
-- given a set of invariants, a preamble, a set of globals, and a body to check, either return the body (if it passes boogie test) or return more counterexamples
checkConfig :: [Expression] -> Program -> [String] -> Config -> Body -> Block -> Either Config (Map.Map String Value)
checkConfig invs (Program header) globals config (vs, _) blk = result
  where
    prog = debugProg "cegis-test-debug.bpl" $ optimize $ Program $ header ++ [main] ++ buildConfVal config
    bod = (vs, saveLocals:blk)
    saveLocals = Pos.gen ([], Pos.gen $ Predicate [Attribute "save" (vs >>= buildAV)] clause)
    buildAV itws = [EAttr $ Pos.gen $ Var $ itwId itw | itw <- itws]
    clause = SpecClause Inline False $ Pos.gen tt
    main = buildMain invs globals bod 
    result = case boogTest prog "Main" of 
      []    -> Left config
      x:xs  -> Right $ buildIO x 

-- given invs and a set of io, add assumes to the begin/end of the block to specialize to the IO.
addAssumes :: [Expression] -> Map.Map String Value -> Block -> Block
addAssumes invs io block = map buildIO (Map.toList io) ++ invStmts ++ block'
  where
    --Predicate attrs (SpecClause _ isAssume e)
    invStmts = map buildInv invs
    buildInv :: Expression -> LStatement
    buildInv e = stmt $ Predicate [] (SpecClause Inline True e)
    buildIO :: (String, Value) -> LStatement
    buildIO ioPr = stmt $ Predicate [] (SpecClause Inline True $ buildExpr ioPr)
    buildExpr :: (String, Value) -> Expression
    buildExpr (l, r@IntValue{}) = eq (Var l) (Literal r)
    buildExpr (l, r@BoolValue{}) = eq (Var l) (Literal r)

    block' = if null block then block ++ invStmts else
      case last block of 
        (Pos.Pos o (ls, Pos.Pos i (If c t f))) -> init block ++ [Pos.Pos o (ls, Pos.Pos i (If c (t ++ invStmts) Nothing))]
        _ -> block ++ invStmts

eq :: BareExpression -> BareExpression -> Expression
eq l r = Pos.gen $ BinaryExpression Eq (Pos.gen l) (Pos.gen r)

-- build main function for exec phase
buildMainSearch :: [String] -> [String] -> Program -> Program
buildMainSearch globals functions (Program decs) = Program $ decs ++ [Pos.gen $ ProcedureDecl "Main" [] [] [] [Modifies False globals] (Just bod)]
  where
    bod = ([], havoc `intersperse` map buildCalls functions)
    buildCalls fname = stmt $ Call [] fname []
    havoc = stmt $ Havoc globals

-- build main function for test/verify phases
buildMain :: [Expression] -> [String] -> Body -> Decl
buildMain invs globals (vs, bod) = Pos.gen $ ProcedureDecl "Main" [] [] [] modifies $ Just (vs, reqs ++ bod ++ ensures)
  where 
    buildClause :: Bool -> Expression -> LStatement
    buildClause b e = Pos.gen ([], Pos.gen $ Predicate [] $ SpecClause Inline b e)
    reqs = map (buildClause True) invs
    ensures = map (buildClause False) invs
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

-- run in exec mode, searching for a valid configuration.
boogExec :: Program -> String -> [TestCase]
boogExec p pname = case typeCheckProgram p of
              Left typeErrs -> error "inconceivable"
              Right context -> runInt context
  where 
    runInt ctx = take 1 $ filter keep . maybeTake exec_max $ int p ctx pname

    int = interpreter branch_max rec_max loop_max minimize concretize True

    branch_max = Nothing   
    exec_max = Nothing --Just 4096
    rec_max = Nothing 
    loop_max = Nothing 
    minimize = True
    concretize = False
    maybeTake = \case
      Nothing -> id
      Just n -> take n
    keep tc = (not . isInvalid) tc && (not . isNonexecutable) tc && (not . isFail) tc

-- run in test mode, searching for an invalid configuration.
boogTest :: Program -> String -> [TestCase]
boogTest p pname = case typeCheckProgram p of
              Left typeErrs -> error "inconceivable"
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
    buildMap = Map.mapMaybe (\case (Pos.Pos _ e) -> eval' e)


-- TODO: mem doesn't actually have local values because...assert violated, i think.
-- the actual values are in logical variables, which we have to dig out.
-- see line 249 of interpreter.hs for clues.
buildIO :: TestCase -> Map.Map String Value
buildIO tc@(TestCase _ mem conMem (Just rtf)) = buildMap ((mem'^.memOld) `Map.union` initLocalVals) -- we use memOld instead of memGlobals because memGlobals has the *failure state* -- we actually want the initial inputs
  where
    mem' :: Memory
    mem' = userMemory conMem mem
    initLocalVals :: Map.Map String Expression
    initLocalVals = Map.mapMaybeWithKey (evalLogicals (mem^.memLogical) (rtfMemory rtf ^. memLocals)) (rtfMemory rtf ^.memLocalOld)
    evalLogicals :: Solution -> Store -> Id -> Thunk -> Maybe Thunk
    evalLogicals logic local var (Pos.Pos x (Logical _ v)) 
      | Map.member v logic    = Just $ Pos.Pos x $ Literal $ (Map.!) logic v 
      | Map.member var local  = Just $ (Map.!) local var 
      | otherwise             = error $ "inconceivable" ++ var
    evalLogicals _ _ _ _ = Nothing -- filter out maps... not sure if smart


    -- takeBaseVal :: Thunk -> Maybe Thunk
    -- takeBaseVal t@Logical{} = Nothing
    -- takeBaseVal 
    buildMap :: Map.Map String Expression -> Map.Map String Value
    buildMap mp = Map.mapMaybe eval' $ Map.mapMaybe worker mp
    worker x@(Pos.Pos _ e@(Literal (IntValue _))) = Just e
    worker _ = Nothing -- filter out unconstrained, irrelevant values; i.e. variables with logical RHS in which logical hasn't been instantiated


interpreter :: Maybe Int -> Maybe Int ->  Maybe Int -> Bool -> Bool -> Bool -> (Program -> Context -> Id -> [TestCase])
interpreter branch_max rec_max loop_max minimize concretize solvePassing p context proc_ = toList $ executeProgram p context solver rec_max loop_max concretize solvePassing generator proc_
  where
    solver :: Solver Logic
    solver = Z3.solver minimize branch_max
    generator = exhaustiveGenerator Nothing