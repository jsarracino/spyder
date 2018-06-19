
module Language.Spyder.Translate.Direct (
    translateExpr
  , translateBlock
  , translateStmt
  , eval'
  , unvalue
  , asInt
  , translateProg
  , mangleFunc
) where


import Language.Spyder.AST
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
import Data.List                                  (find, (\\))
import Data.Maybe

import qualified Language.Boogie.AST as BST
import qualified Language.Boogie.Position as Pos
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Language.Spyder.Util


saturateLoops :: [Set.Set String] -> MainDecl -> MainDecl
saturateLoops rels (ProcDecl nme formals (Imp.Seq ss)) = ProcDecl nme formals $ Imp.Seq ss'
  where
    ss' = map worker ss
    worker :: Imp.Statement -> Imp.Statement
    worker (Imp.For vs idx arrs (Imp.Seq bod)) = Imp.For vs' idx arrs' $ Imp.Seq bod'
      where
        names' = map fst vs'
        arrNames = map takeName arrs
        neededArrs = (computeRels arrNames rels) \\ arrNames
        vs' = vs ++ replicate (length neededArrs) "loop_var" `zip` tys
        tys = repeat Imp.IntTy  -- TODO: real type checking
        arrs' = arrs ++ map Imp.VConst neededArrs
        bod' = map worker bod
    worker (Imp.Cond c (Imp.Seq l) (Imp.Seq r)) = Imp.Cond c (Imp.Seq $ map worker l) (Imp.Seq $ map worker r)
    worker (Imp.While c (Imp.Seq ss)) = Imp.While c $ Imp.Seq (map worker ss)
    worker x = x

    takeName (Imp.VConst v) = v

saturateLoops _ x = x

translateBlock :: (Imp.Block, [BST.IdTypeWhere])  -> (BST.Block, [BST.IdTypeWhere])
translateBlock (Imp.Seq ss, vs) = foldl worker ([], vs) ss
  where 
    worker :: (BST.Block, [BST.IdTypeWhere]) -> Imp.Statement -> (BST.Block, [BST.IdTypeWhere])
    worker (olds, vs) s = (olds ++ news, vs')
      where 
        (bareS, vs') = translateStmt (s, vs)
        news = map (\t -> Pos.gen ([], Pos.gen t)) bareS

-- for loops, we need to insert multiple statements, so we return a list.
-- also, we need to introduce variables, so we plumb a scope around.
translateStmt :: (Imp.Statement, [BST.IdTypeWhere]) -> ([BST.BareStatement], [BST.IdTypeWhere])
-- convert decls into a variable declaration and an assignment. program is NOT alpha-renamed, so name-collisions are a problem.
translateStmt (Imp.Decl (v, ty) rhs, vs) = (maybeToList assn, vs'')
  where 
    assn = (\e -> BST.Assign [(v, [])] [transWithGen e]) `fmap` rhs
    (v', vs') = allocFreshLocal v (translateTy ty) vs
    vs'' = if v /= v' then error "name clash in decl" else vs'
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
translateStmt (Imp.For vs idxDec arrs bod, vars) = 
    (idxInit : dimInit ++ [BST.While (BST.Expr cond) spec (loopInfo ++ iterUpdate ++ loopStart ++ bod' ++ loopEnd ++ arrUpdate ++ [idxUpdate])], vars')
  where
    decls = vs `zip` arrs
    
    --loopVars :: [BST.IdTypeWhere]
    (idx, withIdx) = allocFreshLocal "__loop_idx" BST.IntType vars
    (newVars, lvars) = foldl buildLVars (withIdx, []) decls
    -- allocate new variables for the loop vars. keep track of the names for later.
    buildLVars :: ([BST.IdTypeWhere], [String]) -> (Imp.VDecl, Imp.Expr) -> ([BST.IdTypeWhere], [String])
    buildLVars (vs, lvars) ((name, ty), _) = (vs', lvars ++ [lvar])
      where 
        (lvar, vs') = allocFreshLocal name (translateTy ty) vs
      
    -- TODO: for arrays, need to allocate new length variables for each array, as well as assign the upper n-1 dims of RHS
    -- to the LHS
    -- e.g. if RHS is [[1,2],[3,4]], and LHS is foo, foo$dim0 := 2.

    depth (Imp.ArrTy i) = 1 + depth i
    depth _ = -1

    dimInfo = zip3 lvars (map snd vs) arrs

    -- allocate new variables for loop var dimensions.
    allocDims :: ([BST.IdTypeWhere], [[String]]) -> (String, Imp.Type, Imp.Expr) -> ([BST.IdTypeWhere], [[String]])
    allocDims (vs, dimvs) (lvar, lvTy, Imp.VConst arrv) = (vs', dimvs ++ [lDims])
      where
        dimNames = map (\i -> lvar ++ "$dim" ++ show i) [0..depth lvTy]
        

        (vs', lDims) = foldl worker (vs, []) dimNames

        worker :: ([BST.IdTypeWhere], [String]) -> String -> ([BST.IdTypeWhere], [String])
        worker (vs'', acc) nme = 
          let (rname, rvs) = allocFreshLocal nme BST.IntType vs'' in
            (rvs, acc ++ [rname])

    (newVars', allDims) = foldl allocDims (newVars, []) dimInfo
    spec = []

    liftLS :: BST.BareStatement -> BST.LStatement
    liftLS s = Pos.gen ([], Pos.gen s)

    idxInit = buildInit idx
    dimInit = dimInfo >>= buildDims
    idxUpdate = (liftLS . buildIdxUp) idx
    iterUpdate = map (liftLS . buildIterUp) decls
    arrUpdate = map (liftLS . buildArrUp) decls


    buildInit :: String -> BST.BareStatement
    buildInit name = BST.Assign [(name, [])] [Pos.gen $ BST.numeral 0]
    buildIdxUp :: String -> BST.BareStatement
    buildIdxUp name = BST.Assign [(name, [])] [Pos.gen $ BST.BinaryExpression BST.Plus (Pos.gen $ BST.Var name) (Pos.gen $ BST.numeral 1)]
    buildIterUp :: (Imp.VDecl, Imp.Expr) -> BST.BareStatement
    buildIterUp ((l, _), r) = BST.Assign [(l, [])] [Pos.gen $ BST.MapSelection (transWithGen r) [Pos.gen $ BST.Var idx]]
    buildArrUp :: (Imp.VDecl, Imp.Expr) -> BST.BareStatement
    buildArrUp ((r, _), Imp.VConst l) = BST.Assign [(l, [[Pos.gen $ BST.Var idx]])] [Pos.gen $ BST.Var r]

    buildDims :: (String, Imp.Type, Imp.Expr) -> [BST.BareStatement]
    buildDims (nme, ty, Imp.VConst arr) = [BST.Assign [(nme ++ "$dim" ++ show suf, [])] [Pos.gen $ BST.Var $ arr ++ "$dim" ++ show (suf + 1)] | suf <- dims]
      where dims = [0..depth ty]

    idxSub = Map.fromList $ case idxDec of
      Just idxVar -> [(idxVar, idx)]
      Nothing     -> []

    (bod', vars') = translateBlock (alphaBlock idxSub bod, newVars')

    cond = foldl buildCond (Pos.gen BST.tt) arrs

    buildCond :: BST.Expression -> Imp.Expr -> BST.Expression
    buildCond e (Imp.VConst arr) = Pos.gen $ BST.BinaryExpression BST.And e (Pos.gen $ idx `lt` arr)
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

    arrNames = map extrName arrs
    extrName (Imp.VConst v) = v

    loopInfo = buildPred "forInfo" $ idx : (lvars ++ arrNames)
    loopStart = buildPred "forBegin" []
    loopEnd = buildPred "forEnd" []

    buildPred lab sargs = [liftLS $ BST.Predicate [BST.Attribute lab $ map BST.SAttr sargs] assertTT]
    assertTT = BST.SpecClause BST.Inline False $ Pos.gen BST.tt

translateStmt (Imp.Assgn (Imp.VConst lid) rhs, vars) = ([BST.Assign [(lid, [])] [transWithGen rhs]], vars)
translateStmt (Imp.Assgn lhs rhs, vars) = ([BST.Assign [(lid, largs)] [transWithGen rhs]], vars)
  where
    (lid, lacc) = simplArrAccess lhs
    largs = [map transWithGen lacc]
translateStmt (Imp.While c bod, vars) = ([BST.While (BST.Expr c') [] bod'], vars')
  where
    c' = transWithGen c
    (bod', vars') = translateBlock (bod, vars)
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
    (body', decs) = translateBlock (Imp.Seq bod, [])
    decs' = map (\x -> [x]) decs
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
type CompileState = ([Spec.RelExpr], Map.Map String String)
translateProg :: Program -> (BST.Program, CompileState)
translateProg prog@(comps, MainComp decls) = (debugProg "compile-debug.bpl" outProg, outState)
  where 
    globalVars = mangleVars "Main" $ gatherDDecls decls
    varMap = Map.fromList $ zipWith stripTy2 (gatherDDecls decls) globalVars
    stripTy2 (l, _) (r, _) = (l, r)
    vDecls = globalVars >>= translateVDecl

    comps' = map (processUsing varMap comps) (filter takeUsing decls) 
    relDecls = (comps' `zip` [0..]) >>= translateRels

    relSigs = Map.unions $ map genSig (comps' `zip` [0..])

    invs = (comps' `zip` [0..]) >>= buildInvs relSigs

    

    procs = map (alphaProc varMap) $ filter takeProcs decls

    procDecls = map (translateProc . saturateLoops (relatedVars prog)) procs

    withModifies = map (addModifies (map fst globalVars)) procDecls
    withRequires = map (addRequires invs) withModifies
    withContracts = map (addEnsures invs) withRequires

    outState = (map fst invs, varMap)
    outProg = BST.Program $ map Pos.gen $ vDecls ++ relDecls ++ withContracts


    takeUsing MainUD{} = True
    takeUsing _ = False
    takeProcs ProcDecl{} = True
    takeProcs _ = False

buildInvs :: Map.Map String ([String], Spec.RelExpr) -> (Component, Int) -> [(Spec.RelExpr, [String])]
buildInvs funcs (DerivComp nme decs, x) = exprs `zip` dims
  where
    preExprs = mapMaybe takeAlways decs 
    exprs = map buildExpr preExprs
    dims = map extractDims preExprs

    takeAlways (InvClaus e) = Just $ prefixApps prefix e
    takeAlways _ = Nothing

    prefix = mangleFunc nme x

    buildExpr = inlineApps funcs . saturateApps 
    extractDims (Spec.RelApp f (Spec.RelVar a:_)) = map (\s -> a ++ "$" ++ s) $ dimVars bod
      where 
        bod = snd $ (Map.!) funcs f

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
    Nothing -> undefined "couldn't find the used component"
  where
    usedComp = find takeNme comps
    takeNme (DerivComp n _) = n == nme
    -- two steps: rename the concrete args using vs, and then rename the component using the new args
    args' = map (vs Map.!) args
    renamedComp c  = case instantiate args' c of (DerivComp nme decs) -> DerivComp nme decs

-- convert an array lvalue to an identifier and list of arguments
simplArrAccess :: Imp.Expr -> (String, [Imp.Expr])
simplArrAccess e = worker (e, [])
  where worker (Imp.VConst s, args) = (s, args)
        worker (Imp.Index l r, args) = worker (l, r:args)
        worker (x, _) = undefined $ "tried to convert to array access: " ++ show x

        
        
-- I could just use eval, but I'm not sure how the monads work, and we shouldn't
-- get any free variables in these expressions anyway (because it's a thunk)
eval' :: BST.BareExpression -> BST.Value
eval' (BST.Literal v) = v
eval' (BST.Logical ty val) = error "logical in eval'"
eval' (BST.Var v) = error "var in eval'"
eval' _ = error "inconceivable"

unvalue :: BST.Value -> BST.Expression
unvalue v@BST.IntValue{} = Pos.gen $ BST.Literal v
unvalue _ = undefined "TODO"

asInt :: BST.Value -> Int 
asInt (BST.IntValue v) = fromIntegral v
asInt _ = error "inconceivable"

gatherDDecls :: [MainDecl] -> [Imp.VDecl]
gatherDDecls decs = map unwrap $ filter takeDD decs
  where
    takeDD MainDDecl{} = True
    takeDD _ = False
    unwrap (MainDDecl decs) = decs
    unwrap _ = error "unexpected argument to unwrap"