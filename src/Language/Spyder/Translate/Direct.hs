{-# LANGUAGE LambdaCase #-}

module Language.Spyder.Translate.Direct (
    translateExpr
  , translateBlock
  , translateStmt
  , eval'
  , unvalue
  , asInt
  , translateProg
) where


import Language.Spyder.AST
import qualified Language.Spyder.AST.Imp    as Imp
import qualified Language.Spyder.AST.Spec   as Spec
import Language.Spyder.AST.Component
import Language.Spyder.Translate.Specs

import Language.Spyder.Translate.Desugar
import Language.Spyder.Translate.Rename

import Language.Spyder.Synth.Verify

import Language.Spyder.Translate.Derived          (instantiate)
import Data.List                                  (find)

import qualified Language.Boogie.AST as BST
import qualified Language.Boogie.Position as Pos
import qualified Data.Map.Strict as Map
import Language.Spyder.Util

translateBop :: Imp.Bop -> BST.BinOp
translateBop = \case
  Imp.Plus -> BST.Plus
  Imp.Minus -> BST.Minus
  Imp.Mul -> BST.Times
  Imp.Div -> BST.Div
  Imp.Lt -> undefined "Error: translation assumes LT has been desugared"
  Imp.Le -> BST.Leq
  Imp.Gt -> BST.Gt
  Imp.Ge -> BST.Geq
  Imp.And -> BST.And
  Imp.Or -> BST.Or
  Imp.Eq -> BST.Eq
  Imp.Neq -> BST.Neq

translateUop :: Imp.Uop -> BST.UnOp
translateUop = \case
  Imp.Neg -> BST.Neg
  Imp.Not -> BST.Not

translateTy :: Imp.Type -> BST.Type
translateTy (Imp.BaseTy "int") = BST.IntType
translateTy (Imp.BaseTy "bool") = BST.BoolType
translateTy (Imp.BaseTy _) = undefined "Error: bad type tag"
-- huh. i think this code, and the index code, don't play well...
  -- the index code converts a[x][y] => a[x,y], while this converts
  -- int[][] to [int][int]int, which should be indexed like a[x][y]
translateTy (Imp.ArrTy inner) = BST.MapType [] [BST.IntType] $ translateTy inner
  
translateVDecl :: Imp.VDecl -> [BST.BareDecl]
-- array variables are themselves, plus variables for dimensions
translateVDecl x@(v, ty@Imp.ArrTy{}) = (BST.VarDecl [translateITW x]) : map buildVar dims
  where
    dims = [0..depth ty]
    depth (Imp.ArrTy i) = 1 + depth i
    depth _ = -1

    buildVar n = BST.VarDecl [translateITW (nme, Imp.BaseTy "int")]
      where nme = v ++ "$dim" ++ show n
-- simple variables are just themselves
translateVDecl v = [BST.VarDecl [translateITW v]]

  
transWithGen :: Imp.Expr -> BST.Expression
transWithGen = Pos.gen . translateExpr

translateExpr :: Imp.Expr -> BST.BareExpression
translateExpr (Imp.VConst s) = BST.Var s
translateExpr (Imp.IConst i) = BST.numeral $ toInteger i
translateExpr (Imp.BConst b) = (BST.Literal . BST.BoolValue) b
translateExpr (Imp.BinOp Imp.Lt l r) = BST.UnaryExpression BST.Not $ Pos.gen $ BST.BinaryExpression BST.Geq l' r'
  where (l', r') = (transWithGen l, transWithGen r)
translateExpr (Imp.BinOp o l r) = BST.BinaryExpression op l' r'
  where op = translateBop o
        (l', r') = (transWithGen l, transWithGen r)
translateExpr (Imp.UnOp o i) =
  BST.UnaryExpression (translateUop o) (transWithGen i)
translateExpr (Imp.Index ar i) =
  BST.MapSelection (transWithGen ar) [transWithGen i]
translateExpr (Imp.App (Imp.VConst f) r) = BST.Application f (map transWithGen r)
translateExpr (Imp.AConst _) = undefined "Error: translation assumes array constants have been desugared"

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
-- we can do this now TODO
translateStmt (Imp.Decl _ _, vs) = error "Error: translation assumes decls are lifted"
-- convert 
--  for (vs) <- (arrs) {
--    bod 
--  } 

--  idx := 0;
--  while (BIG_AND {idx < len_i}) {
--    assert {:for-info idx vs arrs } true;
--    {v_i := arr_i[idx];}
--    assert {:for-begin} true;
--    bod;
--    assert {:for-end} true;
--    {arr[idx] := v_i;}
--    idx := idx + 1;
--  }
-- assumes length is stored at arr[-1]
translateStmt (Imp.For vs arrs bod, vars) = 
    (idxInit : [BST.While (BST.Expr cond) spec (loopInfo ++ iterUpdate ++ loopStart ++ bod' ++ loopEnd ++ arrUpdate ++ [idxUpdate])], vars')
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

    dimInfo = zip3 lvars (map snd vs) arrs

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
    buildDims (nme, ty, Imp.VConst arr) = [] -- TODO


    (bod', vars') = translateBlock (bod, newVars)

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
translateProc (ProcDecl nme formals rt body) = BST.ProcedureDecl nme [] formals' [] inv $ Just (decs', body')
  where
    formals' = map translateITW formals
    (vs, bod) = generateBoogieBlock body
    (body', decs) = translateBlock (Imp.Seq bod, map translateITW vs)
    decs' = map (\x -> [x]) decs
    inv = []

translateRels :: (Component, Int) -> [BST.BareDecl]
translateRels (DerivComp nme decs, x) = map (buildRel $ mangleFunc nme x) $ filter takeRD decs
  where
    takeRD RelDecl{} = True
    takeRD _ = False

translateITW :: Imp.VDecl -> BST.IdTypeWhere
translateITW (v, t) = BST.IdTypeWhere v (translateTy t) (Pos.gen BST.tt)

-- returns the program, as well as the:
    -- compiled invariants
    -- a map from old variable names to new variable names
type CompileState = ([BST.Expression], Map.Map String String)
translateProg :: Program -> (BST.Program, CompileState)
translateProg prog@(comps, MainComp decls) = (debugProg "compile-debug.bpl" outProg, outState)
  where 
    globalVars = mangleVars "Main" $ gatherDDecls decls
    varMap = Map.fromList $ zipWith stripTy2 (gatherDDecls decls) globalVars
    stripTy2 (l, _) (r, _) = (l, r)
    vDecls = globalVars >>= translateVDecl

    comps' = map (processUsing varMap comps) (filter takeUsing decls) 
    relDecls = (comps' `zip` [0..]) >>= translateRels

    invs = (comps' `zip` [0..]) >>= buildInvs

    

    procs = map (alphaProc varMap) $ filter takeProcs decls

    procDecls = map translateProc procs

    withModifies = map (addModifies $ map fst globalVars) procDecls
    withRequires = map (addRequires invs) withModifies
    withContracts = map (addEnsures invs) withRequires

    outState = (invs, varMap)
    outProg = BST.Program $ map Pos.gen $ vDecls ++ relDecls ++ withContracts


    takeUsing MainUD{} = True
    takeUsing _ = False
    takeProcs ProcDecl{} = True
    takeProcs _ = False

buildInvs :: (Component, Int) -> [BST.Expression]
buildInvs (DerivComp nme decs, x) = map (buildExpr $ mangleFunc nme x) alwaysDecs
  where
    takeAlways InvClaus{} = True
    takeAlways _ = False
    buildExpr pref (InvClaus e) = specToBoogie [] $ (saturateApps . prefixApps pref) e
    alwaysDecs = filter takeAlways decs
    
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

addRequires :: [BST.Expression] -> BST.BareDecl -> BST.BareDecl
addRequires invs (BST.ProcedureDecl nme tyargs formals rets contract bod) = 
  BST.ProcedureDecl nme tyargs formals rets (contract ++ map buildReq invs) bod
  where
    buildReq = BST.Requires False
addRequires _ v@_ = v

addEnsures :: [BST.Expression] -> BST.BareDecl -> BST.BareDecl
addEnsures invs (BST.ProcedureDecl nme tyargs formals rets contract bod) = 
  BST.ProcedureDecl nme tyargs formals rets (contract ++ map buildReq invs) bod
  where
    buildReq = BST.Ensures False
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
eval' _ = undefined "TODO"

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