
module Language.Spyder.Translate.Direct (
    translateExpr
  , translateBlock
  , translateStmt
  , eval'
  , unvalue
  , asInt
  , translateProg
  , mangleFunc
  , emptyScope
  , buildScope
  , TransScope(..)
) where


import Language.Spyder.AST                  hiding (vars)
import qualified Language.Spyder.AST.Imp    as Imp
import qualified Language.Spyder.AST.Spec   as Spec
import Language.Spyder.AST.Component
import Language.Spyder.Translate.Specs

import Language.Spyder.Translate.Desugar
import Language.Spyder.Translate.Rename
import Language.Spyder.Translate.Related

import Language.Spyder.Synth.Verify
import Language.Spyder.Translate.Expr

import Language.Spyder.Translate.Derived          (instantiate)
import Data.List                                  
import Data.Maybe

import qualified Language.Boogie.AST as BST
import qualified Language.Boogie.Position as Pos
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Language.Spyder.Util

data TransScope = Scope {tys :: Map.Map String Imp.Type, vars :: [BST.IdTypeWhere]}

emptyScope :: TransScope
emptyScope = Scope Map.empty []

buildScope :: DimEnv -> [BST.IdTypeWhere] -> TransScope
buildScope dims = Scope (Map.map buildTy dims)


translateBlock :: (Imp.Block, TransScope)  -> (BST.Block, TransScope)
translateBlock (Imp.Seq ss, vs) = foldl worker ([], vs) ss
  where 
    worker :: (BST.Block, TransScope) -> Imp.Statement -> (BST.Block, TransScope)
    worker (olds, scope) s = (olds ++ news, scope')
      where 
        (bareS, scope') = translateStmt (s, scope)
        news = map (\t -> Pos.gen ([], Pos.gen t)) bareS



-- for loops, we need to insert multiple statements, so we return a list.
-- also, we need to introduce variables, so we plumb a scope around.
translateStmt :: (Imp.Statement, TransScope) -> ([BST.BareStatement], TransScope)
-- convert decls into a variable declaration and an assignment. program is NOT alpha-renamed, so name-collisions are a problem.
translateStmt (Imp.Decl (v, ty) rhs, scope) = (maybeToList assn, scope')
  where 
    assn = (\e -> BST.Assign [(v, [])] [transWithGen e]) `fmap` rhs
    vs = vars scope

    (v', vs') = allocFreshLocal v (translateTy ty) vs
    vs'' = if v /= v' then error "name clash in decl" else vs'

    tys' = Map.insert v' ty (tys scope)

    scope' = Scope tys' vs''
-- convert 
--  for (vs) (with given_idx) in (arrs) {
--    bod
--  } 

--  idx := 0;
--  while (BIG_AND {idx < len_i}) {
--    assert {:for-info idx vs arrs } true;
--    {v_i := arr_i[idx];}
--    assert {:for-begin} true;
--    bod[given_idx/idx];
--    assert {:for-end} true;
--    {arr[idx] := v_i;}
--    idx := idx + 1;
--  }
-- assumes length is stored at arr[-1]
translateStmt (Imp.For decls idxDec bod, scope) = 
    (idxInit : dimInit ++ [BST.While (BST.Expr cond) [] (loopInfo ++ [idxPos] ++ iterUpdate ++ loopStart ++ bod' ++ loopEnd ++ arrUpdate ++ [idxUpdate])], scope')
  where
    (vs, arrs) = unzip decls
    --loopVars :: [BST.IdTypeWhere]
    (idx, withIdx) = allocFreshLocal "__loop_idx" BST.IntType (vars scope)
    (newVars, lvars) = foldl buildLVars (Scope (Map.insert idx Imp.IntTy (tys scope)) withIdx, []) decls
    -- allocate new variables for the loop vars. keep track of the names for later.
    buildLVars :: (TransScope, [String]) -> (String, String) -> (TransScope, [String])
    buildLVars (scope, lvars) (name, arr) = (scope', lvars ++ [lvar])
      where 
        arrty = (Map.!) (tys scope) arr
        lvty = case arrty of 
          Imp.ArrTy x -> x
          _ -> error "loop iteration over non-loop variable"
        (lvar, vs') = allocFreshLocal name (translateTy lvty) (vars scope)
        scope' = Scope (Map.insert lvar lvty (tys scope)) vs'
      
    -- TODO: for arrays, need to allocate new length variables for each array, as well as assign the upper n-1 dims of RHS
    -- to the LHS
    -- e.g. if RHS is [[1,2],[3,4]], and LHS is foo, foo$dim0 := 2.

    vtys = map ((Map.!) (tys newVars)) lvars

    dimInfo = zip3 lvars vtys arrs

    -- allocate new variables for loop var dimensions.
    allocDims :: (TransScope, [[String]]) -> (String, Imp.Type, String) -> (TransScope, [[String]])
    allocDims (scope, dimvs) (lvar, lvTy, arrv) = (scope', dimvs ++ [lDims])
      where
        dimNames = map (\i -> lvar ++ "$dim" ++ show i) [0..(dim lvTy - 1)]
        

        (scope', lDims) = foldl worker (scope, []) dimNames

        worker :: (TransScope, [String]) -> String -> (TransScope, [String])
        worker (scope'', acc) nme = 
          let (rname, rvs) = allocFreshLocal nme BST.IntType (vars scope'') in
            (Scope (Map.insert rname Imp.IntTy (tys scope'')) rvs, acc ++ [rname])

    (newVars', allDims) = foldl allocDims (newVars, []) dimInfo

    liftLS :: BST.BareStatement -> BST.LStatement
    liftLS s = Pos.gen ([], Pos.gen s)

    idxInit = buildInit idx
    dimInit = dimInfo >>= buildDims
    idxUpdate = (liftLS . buildIdxUp) idx
    idxPos = (liftLS . buildIdxPos) idx
    iterUpdate = map (liftLS . buildIterUp) decls
    arrUpdate = map (liftLS . buildArrUp) decls


    buildInit :: String -> BST.BareStatement
    buildInit name = BST.Assign [(name, [])] [Pos.gen $ BST.numeral 0]
    buildIdxPos :: String -> BST.BareStatement
    buildIdxPos name = BST.Predicate [] $ BST.SpecClause BST.Inline True expr
      where
        expr = Pos.gen $ BST.BinaryExpression BST.Geq (Pos.gen $ BST.Var name) (Pos.gen $ BST.numeral 0)
    buildIdxUp :: String -> BST.BareStatement
    buildIdxUp name = BST.Assign [(name, [])] [Pos.gen $ BST.BinaryExpression BST.Plus (Pos.gen $ BST.Var name) (Pos.gen $ BST.numeral 1)]
    buildIterUp :: (String, String) -> BST.BareStatement
    buildIterUp (l, r) = BST.Assign [(l, [])] [Pos.gen $ BST.MapSelection (transWithGen $ Imp.VConst r) [Pos.gen $ BST.Var idx]]
    buildArrUp :: (String, String) -> BST.BareStatement
    buildArrUp (r, l) = BST.Assign [(l, [[Pos.gen $ BST.Var idx]])] [Pos.gen $ BST.Var r]

    buildDims :: (String, Imp.Type, String) -> [BST.BareStatement]
    buildDims (nme, ty, arr) = [BST.Assign [(nme ++ "$dim" ++ show suf, [])] [Pos.gen $ BST.Var $ arr ++ "$dim" ++ show suf ] | suf <- dims]
      where dims = [0..dim ty - 1]

    idxSub = Map.fromList $ case idxDec of
      Just idxVar -> [(idxVar, idx)]
      Nothing     -> []

    (bod', scope') = translateBlock (alphaBlock idxSub bod, newVars')

    cond = foldl buildCond (Pos.gen BST.tt) arrs

    buildCond :: BST.Expression -> String -> BST.Expression
    buildCond e arr = Pos.gen $ BST.BinaryExpression BST.And e (Pos.gen $ idx `lt` arr)
      where
        lt l r = BST.UnaryExpression BST.Not $ Pos.gen $ BST.BinaryExpression BST.Geq (Pos.gen $ BST.Var l) $ Pos.gen $ len arr
    len :: String -> BST.BareExpression
    len arrName = BST.Var $ arrName ++ "$dim0"

    -- we insert several attributes to decorate the boogie code with metadata about the loop.
    -- specifically:
    --  * an attribute detailing the index, the loop variables, and the array variables
    --  * an attribute demarking the start of the original code
    --  * an attribute demarking the end of the original code

    -- this is for synthesis later

    loopInfo = buildPred "forInfo" $ idx : (lvars ++ arrs)
    loopStart = buildPred "forBegin" []
    loopEnd = buildPred "forEnd" []

    buildPred lab sargs = [liftLS $ BST.Predicate [BST.Attribute lab $ map BST.SAttr sargs] assertTT]
    assertTT = BST.SpecClause BST.Inline False $ Pos.gen BST.tt

translateStmt (Imp.Assgn lid rhs, vars) = ([BST.Assign [(lid, [])] [transWithGen rhs]], vars)
-- translateStmt (Imp.While c bod, vars) = ([BST.While (BST.Expr c') [] bod'], vars')
--   where
--     c' = transWithGen c
--     (bod', vars') = translateBlock (bod, vars)
translateStmt (Imp.Cond c tr fl, vars) = ([BST.If cond ts fls], vars'')
  where
    cond = BST.Expr (transWithGen c)
    (ts, vars') = translateBlock (tr, vars)
    
    (fls, vars'') = case fl of 
      (Imp.Seq []) -> (Nothing, vars')
      (Imp.Seq _) -> let (fs, vs) = translateBlock (fl, vars') in (Just fs, vs)

translateProc :: MainDecl -> BST.BareDecl
translateProc (ProcDecl nme formals body) = BST.ProcedureDecl nme [] formals' [] inv $ Just (decs', body')
  where
    formals' = map translateITW formals
    bod = generateBoogieBlock body
    (body', scope) = translateBlock (Imp.Seq bod, emptyScope)
    decs' = map (\x -> [x]) (vars scope)
    inv = []

translateRels :: (Component, Int) -> [BST.BareDecl]
translateRels (DerivComp nme decs, x) = map (buildRel $ mangleFunc nme x) $ filter takeRD decs
  where
    takeRD RelDecl{} = True
    takeRD _ = False



genSig :: (Component, Int) -> Map.Map String ([String], Spec.RelExpr)
genSig (DerivComp nme decs, x) = Map.fromList $ map worker decs' 
  where
    worker (RelDecl rnme formals bod) = (mangleFunc nme x ++ rnme, (map fst formals, bod))
    decs' = filter takeRD decs

    takeRD RelDecl{} = True
    takeRD _ = False


-- returns the program, as well as the:
    -- compiled invariants
    -- a map from old variable names to new variable names
    -- a map from variables to dimensions
type CompileState = ([Spec.RelExpr], Map.Map String String, DimEnv)
translateProg :: Program -> (BST.Program, CompileState)
translateProg = error "TODO"
-- translateProg prog@(comps, MainComp decls) = (debugProg "compile-debug.bpl" outProg, outState)
--   where 
--     -- globalVars = mangleVars "Main" $ gatherDDecls decls
--     globalVars = gatherDDecls decls
--     varMap = Map.fromList $ zipWith stripTy2 (gatherDDecls decls) globalVars
--     stripTy2 (l, _) (r, _) = (l, r)
--     vDecls = globalVars >>= translateVDecl

--     comps' = map (processUsing varMap comps) (filter takeUsing decls) 
--     relDecls = (comps' `zip` [0..]) >>= translateRels

--     relSigs = Map.unions $ map genSig (comps' `zip` [0..])

--     invs = (comps' `zip` [0..]) >>= buildInvs relSigs

--     globalDims = addDims Map.empty globalVars

--     procs = map (alphaProc varMap) $ filter takeProcs decls

--     procDecls = map (translateProc . completeLoop (relatedVars prog) globalDims) procs

--     withModifies = map (addModifies (map fst globalVars)) procDecls
--     withRequires = map (addRequires invs) withModifies
--     withContracts = map (addEnsures invs) withRequires

--     outState = (map fst invs, varMap, globalDims)
--     outProg = BST.Program $ map Pos.gen $ vDecls ++ relDecls ++ withContracts


--     takeUsing MainUD{} = True
--     takeUsing _ = False
--     takeProcs ProcDecl{} = True
--     takeProcs _ = False

buildInvs :: Map.Map String ([String], Spec.RelExpr) -> (Component, Int) -> [(Spec.RelExpr, [String])]
buildInvs funcs (DerivComp nme decs, x) = exprs `zip` dims
  where
    preExprs = mapMaybe takeAlways decs 
    exprs = map buildExpr preExprs
    dims = map extractDims preExprs

    takeAlways (InvClaus e) = Just $ prefixApps prefix e
    takeAlways _ = Nothing

    prefix = mangleFunc nme x

    buildExpr = inlineApps funcs
    extractDims (Spec.RelApp f args) = map (\s -> topRV ++ "$" ++ s) $ dimVars bod
      where 
        (formals, bod) = (Map.!) funcs f
        
        topIdx = case bod of 
          Spec.Foreach _ _ (rv:_) _ -> elemIndex rv formals
          _                         -> error "need dimension of non-foreach spec"
        topRV = case args !! fromJust topIdx of 
          Spec.RelVar v -> v
          _             -> error "Expected variable argument to always clause"

mangleFunc :: String -> Int -> String
mangleFunc prefix count = prefix ++ "__" ++ show count ++ "_"

mangleVars :: String -> [Imp.VDecl] -> [Imp.VDecl]
mangleVars prefix = map worker 
  where worker (nme, ty) = (prefix++"$"++nme, ty)

buildRel :: String -> DerivDecl -> BST.BareDecl
buildRel prefix (RelDecl nme formals bod) = BST.FunctionDecl [] (prefix ++ nme) [] (formals' ++ dims) retTy body
  where
    formals' = map translateFormal formals
    dimVs = dimVars bod
    dims = map (\v -> (Just v, BST.IntType)) dimVs
    retTy = (Nothing, BST.BoolType )
    body = Just $ specToBoogie dimVs bod
buildRel _ _ = undefined "TODO"


translateFormal :: Imp.VDecl -> BST.FArg
translateFormal (v, t) = (Just v, translateTy t)


addModifies :: [String] -> BST.BareDecl -> BST.BareDecl
addModifies vars (BST.ProcedureDecl nme tyargs formals rets contract bod) = 
  BST.ProcedureDecl nme tyargs formals rets (contract ++ map buildModify vars) bod
  where
    buildModify s = BST.Modifies False [s]
addModifies _ v@_ = v

addRequires :: [(Spec.RelExpr, [String])] -> BST.BareDecl -> BST.BareDecl
addRequires invsWithDims (BST.ProcedureDecl nme tyargs formals rets contract bod) = 
  BST.ProcedureDecl nme tyargs formals rets (contract ++ map buildReq invs') bod
  where
    buildReq = BST.Requires False
    invs' = map (uncurry $ flip specToBoogie) invsWithDims
addRequires _ v@_ = v

addEnsures :: [(Spec.RelExpr, [String])]  -> BST.BareDecl -> BST.BareDecl
addEnsures invsWithDims (BST.ProcedureDecl nme tyargs formals rets contract bod) = 
  BST.ProcedureDecl nme tyargs formals rets (contract ++ map buildReq invs') bod
  where
    buildReq = BST.Ensures False
    invs' = map (uncurry $ flip specToBoogie) invsWithDims
addEnsures _ v@_ = v

-- renamed main vars (orig -> new), components, use, returns component instantiated with args
processUsing :: Map.Map String String -> [Component] -> MainDecl -> Component
processUsing vs comps (MainUD (nme, args)) = case usedComp of 
    Just c  -> renamedComp c 
    Nothing -> error $ "can't find the component named by using: " ++ nme
  where
    usedComp = find takeNme comps
    takeNme (DerivComp n _) = n == nme
    -- two steps: rename the concrete args using vs, and then rename the component using the new args
    args' = map (vs Map.!) args
    renamedComp c  = case instantiate args' c of (DerivComp nme decs) -> DerivComp nme decs

-- -- convert an array lvalue to an identifier and list of arguments
-- simplArrAccess :: Imp.Expr -> (String, [Imp.Expr])
-- simplArrAccess e = worker (e, [])
--   where worker (Imp.VConst s, args) = (s, args)
--         worker (Imp.Index l r, args) = worker (l, r:args)
--         worker (x, _) = error $ "tried to convert to array access: " ++ show x

        
        
-- I could just use eval, but I'm not sure how the monads work, and we shouldn't
-- get any free variables in these expressions anyway (because it's a thunk)
eval' :: BST.BareExpression -> Maybe BST.Value
eval' (BST.Literal v) = Just v
eval' (BST.Logical ty val) = Nothing 
eval' (BST.Var v) = Nothing 
eval' _ = Nothing

unvalue :: BST.Value -> BST.Expression
unvalue v@BST.IntValue{} = Pos.gen $ BST.Literal v
unvalue _ = undefined "TODO"

asInt :: BST.Value -> Int 
asInt (BST.IntValue v) = fromIntegral v
asInt v = error $ "Expected int, got: " ++ show v

gatherDDecls :: [MainDecl] -> [Imp.VDecl]
gatherDDecls decs = map unwrap $ filter takeDD decs
  where
    takeDD MainDDecl{} = True
    takeDD _ = False
    unwrap (MainDDecl decs) = decs
    unwrap _ = error "unexpected argument to unwrap"