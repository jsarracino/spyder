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
translateExpr (AConst _) = undefined "Error: translation assumes array constants have been desugared"

translateBlock :: Block -> BST.Block
translateBlock (Seq ss) = map worker ss
  where worker s = Pos.gen ([], (Pos.gen . translateStmt) s)

translateStmt :: Statement -> BST.BareStatement
translateStmt (Decl _ _) = undefined "Error: translation assumes decls are lifted"
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
toBoogie (comps, MainComp decls) = BST.Program allDecs
  where
    vars = mangleVars "Main" $ gatherDDecls decls
    varMap = Map.fromList $ zipWith stripTy (gatherDDecls decls) vars
    stripTy (l, _) (r, _) = (l, r)
    vDecls = map translateVDecl vars
    comps' = map (processUsing varMap comps) (filter takeUsing decls) 

    -- relNames = fmap getRelNames comps'
    relDecls = comps' >>= translateRels
    procDecls = []
    allDecs = relDecls ++ vDecls ++ procDecls

    takeUsing MainUD{} = True
    takeUsing _ = False






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

translateVDecl :: VDecl -> BST.Decl
translateVDecl (nme, ty) = (Pos.gen . BST.VarDecl) [BST.IdTypeWhere nme (translateTy ty) (Pos.gen BST.tt)]

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

translateRels :: Component -> [BST.Decl]
translateRels (DerivComp nme decs) = map buildRel $ filter takeRD decs
  where
    takeRD RelDecl{} = True
    takeRD _ = False

-- FunctionDecl [Attribute] Id [Id] [FArg] FArg (Maybe Expression) |            -- ^ 'FunctionDecl' @name type_args formals ret body@
buildRel :: DerivDecl -> BST.Decl
buildRel (RelDecl nme formals bod) = Pos.gen $ BST.FunctionDecl [] nme [] formals' retTy body
  where
    formals' = map buildFormal formals
    buildFormal (v, t) = (Just v, translateTy t)
    retTy = (Nothing, BST.BoolType )
    body = Just $ (Pos.gen . buildExpr) bod
    buildExpr (BE i) = i
    buildExpr _ = undefined "TODO"
buildRel _ = undefined "TODO"