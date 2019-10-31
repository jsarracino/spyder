{-# LANGUAGE LambdaCase #-}

module Language.Spyder.Translate.Expr (
    translateExpr
  , translateITW
  , translateTy
  , transWithGen
  , translateVDecl
) where

import qualified Language.Spyder.AST.Imp as Imp
import qualified Language.Boogie.AST as BST
import qualified Language.Boogie.Position as Pos

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
  Imp.Mod -> BST.Mod
  x -> error $ "unknown binop " ++ show x
  -- Imp.Impl -> BST.Impl

translateUop :: Imp.Uop -> BST.UnOp
translateUop = \case
  Imp.Neg -> BST.Neg
  Imp.Not -> BST.Not

translateTy :: Imp.Type -> BST.Type
translateTy Imp.IntTy = BST.IntType
translateTy Imp.BoolTy = BST.BoolType
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

    buildVar n = BST.VarDecl [translateITW (nme, Imp.IntTy)]
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

translateITW :: Imp.VDecl -> BST.IdTypeWhere
translateITW (v, t) = BST.IdTypeWhere v (translateTy t) (Pos.gen BST.tt)