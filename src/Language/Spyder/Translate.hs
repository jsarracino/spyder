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
import qualified Language.Boogie.AST as BST
import qualified Language.Boogie.Position as Pos
import Language.Spyder.Translate.Desugar






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
-- assumes var assignments are lifted...
-- translateStmt (Loop vars inits (Seq ss)) =


toBoogie :: Program -> BST.Program
toBoogie (_, bod) = BST.Program allDecs
  where
    invDecls = []
    -- (decls, bod') = generateBoogieBlock bod
    (decls, bod') = undefined "TODO"
    funDecls = [buildFun decls bod']
    allDecs = invDecls ++ funDecls

buildFun :: [VDecl] -> [Statement] -> BST.Decl
buildFun decs bod = Pos.gen $ BST.ProcedureDecl "main" [] [] [] [] $ Just bod'
  where
    bod' = ([[buildVar v] | v <- decs], translateBlock (Seq bod))
    buildVar (nme, ty) = BST.IdTypeWhere nme (translateTy ty) (Pos.gen BST.tt)

translateTy :: Type -> BST.Type
translateTy (BaseTy "int") = BST.IntType
translateTy (BaseTy _) = undefined "Error: bad type tag"
-- huh. i think this code, and the index code, don't play well...
-- the index code converts a[x][y] => a[x,y], while this converts
-- int[][] to [int][int]int, which should be indexed like a[x][y]
translateTy (ArrTy inner) = BST.MapType [] [BST.IntType] $ translateTy inner
