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
import Language.Spyder.AST.Imp
import Language.Spyder.AST.Spec
import Language.Spyder.AST.Component

import Language.Spyder.Translate.Desugar
import Language.Spyder.Translate.Rename

import Language.Spyder.Synth.Verify

import Language.Spyder.Translate.Derived          (instantiate, prefixApps)
import Data.List                                  (find)

import qualified Language.Boogie.AST as BST
import qualified Language.Boogie.Position as Pos
import qualified Data.Map.Strict as Map
import Language.Spyder.Util

translateBop :: Bop -> BST.BinOp
translateBop = \case
  Plus -> BST.Plus
  Minus -> BST.Minus
  Mul -> BST.Times
  Div -> BST.Div
  Lt -> undefined "Error: translation assumes LT has been desugared"
  Le -> BST.Leq
  Gt -> BST.Gt
  Ge -> BST.Geq
  And -> BST.And
  Or -> BST.Or
  Eq -> BST.Eq
  Neq -> BST.Neq

translateUop :: Uop -> BST.UnOp
translateUop = \case
  Neg -> BST.Neg
  Not -> BST.Not
translateTy :: Type -> BST.Type
translateTy (BaseTy "int") = BST.IntType
translateTy (BaseTy "bool") = BST.BoolType
translateTy (BaseTy _) = undefined "Error: bad type tag"
-- huh. i think this code, and the index code, don't play well...
  -- the index code converts a[x][y] => a[x,y], while this converts
  -- int[][] to [int][int]int, which should be indexed like a[x][y]
translateTy (ArrTy inner) = BST.MapType [] [BST.IntType] $ translateTy inner
  
translateVDecl :: VDecl -> BST.BareDecl
translateVDecl v = BST.VarDecl [translateITW v]

  
transWithGen :: Expr -> BST.Expression
transWithGen = Pos.gen . translateExpr

translateExpr :: Expr -> BST.BareExpression
translateExpr (VConst s) = BST.Var s
translateExpr (IConst i) = BST.numeral $ toInteger i
translateExpr (BConst b) = (BST.Literal . BST.BoolValue) b
translateExpr (BinOp Lt l r) = BST.UnaryExpression BST.Not $ Pos.gen $ BST.BinaryExpression BST.Geq l' r'
  where (l', r') = (transWithGen l, transWithGen r)
translateExpr (BinOp o l r) = BST.BinaryExpression op l' r'
  where op = translateBop o
        (l', r') = (transWithGen l, transWithGen r)
translateExpr (UnOp o i) =
  BST.UnaryExpression (translateUop o) (transWithGen i)
translateExpr (Index ar i) =
  BST.MapSelection (transWithGen ar) [transWithGen i]
translateExpr (App (VConst f) r) = BST.Application f (map transWithGen r)
translateExpr (AConst _) = undefined "Error: translation assumes array constants have been desugared"

translateBlock :: (Block, [BST.IdTypeWhere])  -> (BST.Block, [BST.IdTypeWhere])
translateBlock (Seq ss, vs) = foldl worker ([], vs) ss
  where 
    worker :: (BST.Block, [BST.IdTypeWhere]) -> Statement -> (BST.Block, [BST.IdTypeWhere])
    worker (olds, vs) s = (olds ++ news, vs')
      where 
        (bareS, vs') = translateStmt (s, vs)
        news = map (\t -> Pos.gen ([], Pos.gen t)) bareS

-- for loops, we need to insert multiple statements, so we return a list.
-- also, we need to introduce variables, so we plumb a scope around.
translateStmt :: (Statement, [BST.IdTypeWhere]) -> ([BST.BareStatement], [BST.IdTypeWhere])
-- we can do this now TODO
translateStmt (Decl _ _, vs) = error "Error: translation assumes decls are lifted"
-- convert 
--  for (vs) <- (arrs) {
--    bod 
--  } 

--  {idx_i := 0};
--  while (BIG_AND {idx_i < len_i}) {
--    {v_i := arr_i[idx_i];}
--    bod;
--    {arr[idx_i] := v_i;}
--    {idx_i := idx_i + 1;}
--  }
-- assumes length is stored at arr[-1]
translateStmt (For vs arrs bod, vars) = (idxInit ++ [BST.While (BST.Expr cond) spec (iterUpdate ++ bod' ++ arrUpdate ++ idxUpdate)], vars')
  where
    decls = vs `zip` arrs

    --loopVars :: [BST.IdTypeWhere]
    (newVars, ivars, lvars) = foldl buildLVars (vars, [], []) decls
    -- allocate new variables for the indices, as well as the actual loop vars. keep track of the names for later.
    -- TODO: handle ty
    buildLVars :: ([BST.IdTypeWhere], [String], [String]) -> (VDecl, Expr) -> ([BST.IdTypeWhere], [String], [String])
    buildLVars (vs, idxs, lvars) ((name, _), _) = (vs'', idxs ++ [idx], lvars ++ [lvar])
      where 
        (idx, vs')    = allocFreshLocal "__loop_idx" vs
        (lvar, vs'' ) = allocFreshLocal name vs'
        

    spec = []

    liftLS :: BST.BareStatement -> BST.LStatement
    liftLS s = Pos.gen ([], Pos.gen s)

    idxInit = map buildInit ivars
    idxUpdate = map (liftLS . buildIdxUp) ivars
    iterUpdate = map (liftLS . buildIterUp) (decls `zip` ivars)
    arrUpdate = map (liftLS . buildArrUp) (decls `zip` ivars)


    buildInit :: String -> BST.BareStatement
    buildInit name = BST.Assign [(name, [])] [Pos.gen $ BST.numeral 0]
    buildIdxUp :: String -> BST.BareStatement
    buildIdxUp name = BST.Assign [(name, [])] [Pos.gen $ BST.BinaryExpression BST.Plus (Pos.gen $ BST.Var name) (Pos.gen $ BST.numeral 1)]
    buildIterUp :: ((VDecl, Expr), String) -> BST.BareStatement
    buildIterUp (((l, _), r), i) = BST.Assign [(l, [])] [Pos.gen $ BST.MapSelection (transWithGen r) [Pos.gen $ BST.Var i]]
    buildArrUp :: ((VDecl, Expr), String) -> BST.BareStatement
    buildArrUp (((r, _), VConst l), i) = BST.Assign [(l, [[Pos.gen $ BST.Var i]])] [Pos.gen $ BST.Var r]


    (bod', vars') = translateBlock (bod, newVars)

    cond = foldl buildCond (Pos.gen BST.tt) (ivars `zip` arrs)

    buildCond :: BST.Expression -> (String, Expr) -> BST.Expression
    buildCond e (idx, VConst arr) = Pos.gen $ BST.BinaryExpression BST.And e (Pos.gen $ idx `lt` arr)
      where
        lt l r = BST.UnaryExpression BST.Not $ Pos.gen $ BST.BinaryExpression BST.Geq (Pos.gen $ BST.Var l) $ Pos.gen $ BST.MapSelection (Pos.gen $ BST.Var r) [Pos.gen $ BST.numeral (-1)]
translateStmt (Assgn (VConst lid) rhs, vars) = ([BST.Assign [(lid, [])] [transWithGen rhs]], vars)
translateStmt (Assgn lhs rhs, vars) = ([BST.Assign [(lid, largs)] [transWithGen rhs]], vars)
  where
    (lid, lacc) = simplArrAccess lhs
    largs = [map transWithGen lacc]
translateStmt (While c bod, vars) = ([BST.While (BST.Expr c') spec bod'], vars')
  where
    spec = []
    c' = transWithGen c
    (bod', vars') = translateBlock (bod, vars)
translateStmt (Cond c tr fl, vars) = ([BST.If cond ts fls], vars'')
  where
    cond = BST.Expr (transWithGen c)
    (ts, vars') = translateBlock (tr, vars)
    
    (fls, vars'') = case fl of 
      (Seq []) -> (Nothing, vars')
      (Seq _) -> let (fs, vs) = translateBlock (fl, vars') in (Just fs, vs)

translateProc :: MainDecl -> BST.BareDecl
translateProc (ProcDecl nme formals rt body) = BST.ProcedureDecl nme [] formals' [] inv $ Just (decs', body')
  where
    formals' = map translateITW formals
    (vs, bod) = generateBoogieBlock body
    (body', decs) = translateBlock (Seq bod, map translateITW vs)
    decs' = map (\x -> [x]) decs
    inv = []

translateRels :: (Component, Int) -> [BST.BareDecl]
translateRels (DerivComp nme decs, x) = map (buildRel $ mangleFunc nme x) $ filter takeRD decs
  where
    takeRD RelDecl{} = True
    takeRD _ = False

translateITW :: VDecl -> BST.IdTypeWhere
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
    vDecls = map translateVDecl globalVars

    comps' = map (processUsing varMap comps) (filter takeUsing decls) 
    relDecls = (comps' `zip` [0..]) >>= translateRels

    invs = (comps' `zip` [0..]) >>= buildInvs

    

    procs = map (alphaProc varMap) $ filter takeProcs decls

    procDecls = map translateProc procs

    withModifies = map (addModifies $ map stripTy globalVars) procDecls
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
    buildExpr pref (InvClaus (BE e)) = prefixApps pref e
    alwaysDecs = filter takeAlways decs
    
mangleFunc :: String -> Int -> String
mangleFunc prefix count = prefix ++ "__" ++ show count ++ "_"

mangleVars :: String -> [VDecl] -> [VDecl]
mangleVars prefix = map worker 
  where worker (nme, ty) = (prefix++"$"++nme, ty)

buildRel :: String -> DerivDecl -> BST.BareDecl
buildRel prefix (RelDecl nme formals bod) = BST.FunctionDecl [] (prefix ++ nme) [] formals' retTy body
  where
    formals' = map translateFormal formals
    retTy = (Nothing, BST.BoolType )
    body = Just $ buildExpr bod
    buildExpr (BE i) = i
    buildExpr _ = undefined "TODO"
buildRel _ _ = undefined "TODO"


translateFormal :: VDecl -> BST.FArg
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
-- todo: fix this hack
simplArrAccess :: Expr -> (String, [Expr])
simplArrAccess e = worker (e, [])
  where worker (VConst s, args) = (s, args)
        worker (Index l r, args) = worker (l, r:args)
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

gatherDDecls :: [MainDecl] -> [VDecl]
gatherDDecls decs = map unwrap $ filter takeDD decs
  where
    takeDD MainDDecl{} = True
    takeDD _ = False
    unwrap (MainDDecl decs) = decs
    unwrap _ = error "unexpected argument to unwrap"