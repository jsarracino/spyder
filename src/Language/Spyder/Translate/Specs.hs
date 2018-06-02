{-# LANGUAGE LambdaCase #-}

module Language.Spyder.Translate.Specs (
    prefixApps
  , specToBoogie
) where

import qualified Language.Boogie.AST as BST
import qualified Language.Boogie.Position as Pos
import Language.Spyder.AST.Spec

prefixApps :: String -> RelExpr -> RelExpr
prefixApps pref = recur
  where 
    recur :: RelExpr -> RelExpr
    recur (RelApp f args) = RelApp (pref ++ f) $ map recur args
    recur (RelBinop o l r) = RelBinop o (recur l) (recur r)
    recur (RelUnop o i) = RelUnop o (recur i)
    recur (Foreach vs arrs bod) = Foreach vs arrs $ recur bod
    recur x@RelVar{} = x
    recur x@RelInt{} = x
    recur x@RelBool{} = x
    recur _ = error "TODO"

specBop :: RelBop -> BST.BinOp
specBop = \case
  Plus -> BST.Plus
  Minus -> BST.Minus
  Mul -> BST.Times
  Div -> BST.Div
  Lt -> error "Error: translation assumes LT has been desugared"
  Le -> BST.Leq
  Gt -> BST.Gt
  Ge -> BST.Geq
  And -> BST.And
  Or -> BST.Or
  Eq -> BST.Eq
  Neq -> BST.Neq
  Imp -> BST.Implies
  Iff -> BST.Equiv

specUnop :: RelUop -> BST.UnOp
specUnop = \case
  Neg -> BST.Neg
  Not -> BST.Not

specToBoogie :: RelExpr -> BST.Expression
specToBoogie = recur
  where
    recur :: RelExpr -> BST.Expression
    recur (RelApp f args) = Pos.gen $ BST.Application f $ map recur args
    recur (RelBinop Lt l r) = recur (RelUnop Not $ RelBinop Ge l r)
    recur (RelBinop o l r) = Pos.gen $ BST.BinaryExpression (specBop o) (recur l) (recur r)
    recur (RelUnop o i) = Pos.gen $ BST.UnaryExpression (specUnop o) (recur i)
    recur (Foreach vs arrs bod) = error "TODO" 
    recur (RelVar x) = Pos.gen $ BST.Var x
    recur (RelInt x) = Pos.gen $ BST.Literal $ BST.IntValue $ fromIntegral x
    recur (RelBool x) = Pos.gen $ BST.Literal $ BST.BoolValue x
    recur _ = error "TODO"