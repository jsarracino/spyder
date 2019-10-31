module Language.Spyder.Translate.Rebuild (
    rebuildFix
  , rebuildExpr
) where

import Language.Spyder.AST
-- import Language.Spyder.AST.Component
import qualified Language.Spyder.AST.Imp as Imp
-- import qualified Language.Boogie.AST as BST
import Language.Boogie.AST

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Language.Boogie.Position


import Data.Maybe
import Data.List


rebuildFix :: Block -> Imp.Block
rebuildFix blk = Imp.Seq $ foldl worker [] blk
  where
    worker acc (Pos _ (_, Pos _ (If (Expr e) t f))) = acc ++ [Imp.Cond (rebuildExpr e) (rebuildFix t) (rebuildFix $ fromMaybe [] f)]
    worker acc (Pos _ (_, Pos _ (Assign [(v, _)] [e]))) = acc ++ [Imp.Assgn v (rebuildExpr e)]
    worker acc _ = acc
    -- worker = error "TODO"

rebuildExpr :: Expression -> Imp.Expr
rebuildExpr (Pos _ (Literal (IntValue v))) = Imp.IConst $ fromIntegral v
rebuildExpr (Pos _ (Literal (BoolValue v))) = Imp.BConst v
rebuildExpr (Pos _ (Var v)) = Imp.VConst v
rebuildExpr (Pos _ (BinaryExpression o l r)) = Imp.BinOp o' (rebuildExpr l) (rebuildExpr r)
  where
    -- Plus | Minus | Times | Div | Mod | And | Or | Implies | Explies | Equiv | Eq | Neq | Lc | Ls | Leq | Gt | Geq
    -- =>
    --     Plus | Minus | Mul | Div | Lt | Gt | Le | Ge  | And | Or | Eq | Neq | Mod
    o' = case o of 
      Plus -> Imp.Plus
      Minus -> Imp.Minus
      Times -> Imp.Mul
      Div -> Imp.Div
      Mod -> Imp.Mod
      And -> Imp.And
      Or -> Imp.Or
      Eq -> Imp.Eq
      Neq -> Imp.Neq
      Leq -> Imp.Le
      Gt -> Imp.Gt
      Geq -> Imp.Ge
      otherwise -> error $ "unsupported binop " -- ++ show o
rebuildExpr (Pos _ (UnaryExpression o i)) = Imp.UnOp o' (rebuildExpr i)
  where
    o' = case o of 
      Neg -> Imp.Neg
      Not -> Imp.Not
      otherwise -> error $ "unsupported binop "  -- ++ show o

rebuildExpr e = error $ "unsupported expr " -- ++ show e