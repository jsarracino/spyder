{-# LANGUAGE LambdaCase #-}

module Language.Spyder.Translate.Direct (
    translateExpr
  , translateBlock
  , translateStmt
) where


import Language.Spyder.AST.Imp
import Language.Spyder.AST.Spec

import qualified Language.Boogie.AST as BST
import qualified Language.Boogie.Position as Pos

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



-- convert an array lvalue to an identifier and list of arguments
-- todo: fix this hack
simplArrAccess :: Expr -> (String, [Expr])
simplArrAccess e = worker (e, [])
  where worker (VConst s, args) = (s, args)
        worker (Index l r, args) = worker (l, r:args)
        worker (x, _) = undefined $ "tried to convert to array access: " ++ show x