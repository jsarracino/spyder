{-# LANGUAGE LambdaCase #-}

module Language.Spyder.Translate (
    toBoogie
  , translateExpr
  , translateStmt
  , translateBlock
) where

import Language.Spyder.AST                        (Program)
import Language.Spyder.AST.Imp
import Language.Spyder.AST.Spec
import Language.Spyder.AST.Component              (MainDecl(..), DerivDecl(..), Component(..))
import qualified Language.Boogie.AST as BST
import qualified Language.Boogie.Position as Pos
import Language.Spyder.Translate.Desugar
import Language.Spyder.Translate.Main
import qualified Data.Map.Strict as Map
import Language.Spyder.Translate.Derived          (instantiate)
import Data.List                                  (find)
import Language.Spyder.Translate.Rename

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

transWithGen :: Expr -> BST.Expression
transWithGen = Pos.gen . translateExpr

translateExpr :: Expr -> BST.BareExpression
translateExpr (VConst s) = BST.Var s
translateExpr (IConst i) = BST.numeral $ toInteger i
translateExpr (BConst b) = (BST.Literal . BST.BoolValue) b
translateExpr (BinOp o l r) = BST.BinaryExpression op l' r'
  where op = translateBop o
        (l', r') = (transWithGen l, transWithGen r)
translateExpr (UnOp o i) =
  BST.UnaryExpression (translateUop o) (transWithGen i)
translateExpr (Index ar i) =
  BST.MapSelection (transWithGen ar) [transWithGen i]
translateExpr (App (VConst f) r) = BST.Application f (map transWithGen r)
translateExpr (AConst _) = undefined "Error: translation assumes array constants have been desugared"

translateBlock :: Block -> BST.Block
translateBlock (Seq ss) = map worker ss
  where worker s = Pos.gen ([], (Pos.gen . translateStmt) s)

translateStmt :: Statement -> BST.BareStatement
translateStmt (Decl _ _) = undefined "Error: translation assumes decls are lifted"
translateStmt (Assgn (VConst lid) rhs) = BST.Assign [(lid, [])] [transWithGen rhs]
translateStmt (Assgn lhs rhs) = BST.Assign [(lid, largs)] [transWithGen rhs]
  where
    (lid, lacc) = simplArrAccess lhs
    largs = [map transWithGen lacc]
translateStmt (While c bod) = BST.While (BST.Expr c') spec bod'
  where
    spec = []
    c' = transWithGen c
    bod' = translateBlock bod


toBoogie :: Program -> BST.Program
toBoogie (comps, MainComp decls) = BST.Program (map Pos.gen allDecs)
  where
    vars = mangleVars "Main" $ gatherDDecls decls
    varMap = Map.fromList $ zipWith stripTy2 (gatherDDecls decls) vars
    stripTy2 (l, _) (r, _) = (l, r)
    vDecls = map translateVDecl vars

    comps' = map (processUsing varMap comps) (filter takeUsing decls) 
    relDecls = comps' >>= translateRels


    procs = map (alphaProc varMap) $ filter takeProcs decls


    procDecls = map translateProc procs
    withModifies = map (addModifies $ map stripTy vars) procDecls
    
    invs = comps' >>= buildInvs
    
    withRequires = map (addRequires invs) withModifies
    withContracts = map (addEnsures invs) withRequires

    allDecs = relDecls ++ vDecls ++ withContracts

    takeUsing MainUD{} = True
    takeUsing _ = False
    takeProcs ProcDecl{} = True
    takeProcs _ = False






buildFun :: [VDecl] -> [Statement] -> BST.Decl
-- buildFun decs bod = Pos.gen $ BST.ProcedureDecl "main" [] [] [] [] $ Just bod'
--   where
--     bod' = ([[translateVDecl v] | v <- decs], translateBlock (Seq bod))
buildFun = undefined "TODO"

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

mangleVars :: String -> [VDecl] -> [VDecl]
mangleVars prefix = map worker 
  where worker (nme, ty) = (prefix++"$"++nme, ty)

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
    renamedComp = instantiate args'

getRelNames :: Component -> [String]
getRelNames (DerivComp nme decs) = map worker relDecs
  where
    worker (RelDecl n _ _) = n
    worker _ = undefined "inconceivable"
    relDecs = filter takeRD decs
    takeRD RelDecl{} = True
    takeRD _ = False

translateRels :: Component -> [BST.BareDecl]
translateRels (DerivComp nme decs) = map buildRel $ filter takeRD decs
  where
    takeRD RelDecl{} = True
    takeRD _ = False

    -- ProcedureDecl Id [Id] [IdTypeWhere] [IdTypeWhere] [Contract] (Maybe Body) |  -- ^ 'ProcedureDecl' @name type_args formals rets contract body@
translateProc :: MainDecl -> BST.BareDecl
translateProc (ProcDecl nme formals rt body) = BST.ProcedureDecl nme [] formals' [] inv body'
  where
    formals' = map translateITW formals
    (decs, bod) = generateBoogieBlock body
    -- ([[translateVDecl v] | v <- decs], translateBlock (Seq bod))
    body' = Just ([[translateITW v] | v <- decs], translateBlock $ Seq bod)
    inv = []

buildRel :: DerivDecl -> BST.BareDecl
buildRel (RelDecl nme formals bod) = BST.FunctionDecl [] nme [] formals' retTy body
  where
    formals' = map translateFormal formals
    retTy = (Nothing, BST.BoolType )
    body = Just $ (Pos.gen . buildExpr) bod
    buildExpr (BE i) = i
    buildExpr _ = undefined "TODO"
buildRel _ = undefined "TODO"

translateFormal :: VDecl -> BST.FArg
translateFormal (v, t) = (Just v, translateTy t)
translateITW :: VDecl -> BST.IdTypeWhere
translateITW (v, t) = BST.IdTypeWhere v (translateTy t) (Pos.gen BST.tt)


addModifies :: [String] -> BST.BareDecl -> BST.BareDecl
addModifies vars (BST.ProcedureDecl nme tyargs formals rets contract bod) = 
  BST.ProcedureDecl nme tyargs formals rets (contract ++ map buildModify vars) bod
  where
    buildModify s = BST.Modifies False [s]
addModifies _ v@_ = v

addRequires :: [BST.BareExpression] -> BST.BareDecl -> BST.BareDecl
addRequires invs (BST.ProcedureDecl nme tyargs formals rets contract bod) = 
  BST.ProcedureDecl nme tyargs formals rets (contract ++ map buildReq invs) bod
  where
    buildReq e = BST.Requires False (Pos.gen e)
addRequires _ v@_ = v

addEnsures :: [BST.BareExpression] -> BST.BareDecl -> BST.BareDecl
addEnsures invs (BST.ProcedureDecl nme tyargs formals rets contract bod) = 
  BST.ProcedureDecl nme tyargs formals rets (contract ++ map buildReq invs) bod
  where
    buildReq e = BST.Ensures False (Pos.gen e)
addEnsures _ v@_ = v

buildInvs :: Component -> [BST.BareExpression]
buildInvs (DerivComp _ decs) = map buildExpr alwaysDecs
  where
    takeAlways InvClaus{} = True
    takeAlways _ = False
    buildExpr (InvClaus (BE e)) = e
    alwaysDecs = filter takeAlways decs