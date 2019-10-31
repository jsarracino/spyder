{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}
module Language.Spyder.Bench (
  size,
  treeify,
  implSize,
  specSize,
  specTree,
  implTree
)

where

import Language.Spyder.AST.Spec
import Language.Spyder.AST
import qualified Language.Boogie.AST as BST
import Language.Spyder.AST.Component
import qualified Language.Spyder.AST.Imp as Imp

import Language.Boogie.Position 

class Measurable a where 
  size :: a -> Int

instance Measurable a => Measurable [a] where
  size as = sum $ map size as

instance Measurable a => Measurable (Maybe a) where
  size Nothing = 0
  size (Just x) = size x

-- don't use this; instead, diff at command line
instance Measurable BST.Program where
  size = error "don't use me"


instance Measurable a => Measurable (Pos a) where
  size = size . node

-- instance (Measurable a, Measurable b) => Measurable (a,b) where
--   size (l, r) = size l + size r

instance Measurable BST.BasicBlock where
  size (_, s) = 1 + size s
-- boogBlkSize :: BST.BasicBlock -> Int
-- boogBlkSize 

instance Measurable BST.BareStatement where
  -- don't count havocs and assume/asserts
  size (BST.Predicate _ _) = 0
  size (BST.Havoc _) = 0
  size (BST.Assign _ rhss) = length rhss + size rhss 
  size BST.Call{} = 0 
  size BST.CallForall{} = 0
  size (BST.If e tb mfb) = size e + size tb + size mfb
  size (BST.While e _ b) = size e + size b
  size BST.Break{} = 0
  size BST.Return = 0
  size BST.Goto{} = 0
  size BST.Skip = 0

instance Measurable BST.WildcardExpression where
  size BST.Wildcard = 0
  size (BST.Expr e) = size e
instance Measurable BST.BareExpression where
  size BST.Literal{} = 1
  size BST.Var{} = 1
  size BST.Logical{} = 1
  size (BST.MapSelection l is) = 1 + size l + size is
  size (BST.Application f args) = 2 + size args
  size (BST.MapUpdate mp is rhs) = 1 + size mp + size is + size rhs
  size (BST.Old e) = 1 + size e
  size (BST.IfExpr c t f) = 1 + size c + size t + size f
  size (BST.Coercion e t) = size e + 1
  size (BST.UnaryExpression _ e) = 1 + size e
  size (BST.BinaryExpression _ l r) = 1 + size l + size r
  size (BST.Quantified _ vs tys e) = length vs + length tys + size e

instance Measurable ([BST.Id], BST.Statement) where
  size (vs, s) = length vs + size s
-- size of spyder ast (main less using + relations in components)
-- instance Measurable Program where

--   size :: Program -> Int
--   size (others, main) = size main + sum (map size others)

-- instance Measurable Component where
--   size (MainComp decs) = size decs
--   size (DerivComp _ decs) = size decs

-- -- return a tuple of (invariant, implementation) size
-- ispecSize :: Program -> (Int, Int)
-- ispecSize (others, main) = (size main, size others)

-- instance Measurable MainDecl where

--   size (MainDDecl vd) = 1 + size vd
--   size (MainUD _ ) = 0
--   size (ProcDecl _ fs (Imp.Seq ss)) = 1 + sum (map size fs) + size ss

-- instance Measurable DerivDecl where
--   size (RelDecl _ _ i) = size i
--   size _ = 0

-- instance Measurable Imp.Statement where
--   size (Imp.Decl vd (Just e)) = 1 + size vd + size e
--   size (Imp.Decl vd Nothing) = 1 + size vd
--   size (Imp.Assgn _ e) = 1 + size e
--   size (Imp.For vs (Just _) arrs (Imp.Seq ss)) = 2 + size vs + length arrs + size ss
--   size (Imp.For vs Nothing arrs (Imp.Seq ss)) = 1 + size vs + length arrs + size ss
--   size (Imp.Cond c (Imp.Seq st) (Imp.Seq sf)) = 1 + size c + size (st ++ sf) 

-- instance Measurable Imp.Expr where
--   size (Imp.Index l r) = 1 + size l + size r
--   size (Imp.UnOp _ i) = 1 + size i
--   size (Imp.BinOp _ l r) = 1 + size l + size r
--   size Imp.VConst{} = 1
--   size Imp.IConst{} = 1
--   size Imp.BConst{} = 1

-- instance Measurable Imp.VDecl where
--   size (_,t) = 1 + size t

-- instance Measurable Imp.Type where
--   size (Imp.ArrTy i) = size i + 1
--   size _ = 1

-- instance Measurable RelExpr where
--   size (Foreach ls (Just s) as b) = length (ls ++ as) + 2 + size b
--   size (Foreach ls Nothing as b) = length (ls ++ as) + 1 + size b
--   size (Prev s i) = 2 + size i
--   size (RelUnop _ i) = 1 + size i
--   size (RelBinop _ l r ) = size l + size r + 1
--   -- size (RelIndex l r ) = size l + size r + 1
--   size RelVar{} = 1
--   size RelInt{} = 1
--   size RelBool{} = 1

-- -- count of ast nodes for invariant
-- invSize :: Program -> Int
-- invSize (others, _) = sum (map size others)

-- diffExpr :: Imp.Expr -> Imp.Expr -> Int
-- diffExpr old new = if old == new then 0 else 
--   case (old, new) of 
--     (Imp.Index l r, Imp.Index l' r') -> diffExpr l l' + diffExpr r r'
--     (_, Imp.Index l r) -> size new
--     (Imp.UnOp lo li, Imp.UnOp ro ri) -> if lo == ro then diffExpr li ri else 1 + diffExpr li ri
--     (_, Imp.UnOp _ i) -> 1 + diffExpr new i
--     (Imp.BinOp lo ll lr, Imp.BinOp ro rl rr) -> (if lo == ro then 0 else 1) +  diffExpr ll rl + diffExpr lr rr
--     (_, Imp.BinOp _ l r) -> 1 + min (diffExpr new l + size r) (diffExpr new r + size l)
--     (Imp.VConst l, Imp.VConst r) -> 1
--     (_, Imp.VConst{}) -> 1
--     (Imp.IConst l, Imp.IConst r) -> 1
--     (_, Imp.IConst{}) -> 1
--     (Imp.BConst{}, Imp.BConst{}) -> 1
--     (_, Imp.BConst{}) -> 1
    -- size Imp.VConst{} = 1
    -- size Imp.IConst{} = 1
    -- size Imp.BConst{} = 1



data Tree = Tree String [Tree] deriving (Eq, Ord)

class Treeify a where
  treeify :: a -> Tree

instance Treeify Tree where
  treeify = id

-- instance Measurable Tree where
--   size (Tree (_,ts)) = 1 + sum (map size ts)

instance {-# OVERLAPPING #-} (Treeify a) => (Measurable a) where
  size :: a -> Int
  size x = case treeify x of
    (Tree _ ts) -> 1 + sum (map size ts)

instance Show Tree where
  show (Tree label kids) = "{" ++ label ++ concatMap show kids ++ "}"


instance Treeify Program where
  treeify prog = Tree "Program" [specTree prog, implTree prog]

specTree :: Program -> Tree
specTree (others, _) = Tree "Specs" $ map treeify others

implTree (_, main) = Tree "Impl" [treeify main]

instance Treeify Component where
  treeify (MainComp decs) = Tree "Impl" $ map treeify $ filter use decs
    where
      use MainDDecl{} = True
      use MainUD{} = False
      use ProcDecl{} = True
  treeify (DerivComp _ decs) = Tree "Spec" $ map treeify $ filter use decs
    where 
      use RelDecl{} = True
      use _ = False

instance Treeify MainDecl where
  treeify (MainDDecl vd) = Tree "DataDecl" [treeify vd]
  treeify (MainUD _ ) = error "unreachable"
  treeify (ProcDecl name fs (Imp.Seq ss)) = Tree ("Procedure" ++ name) $
    Tree "args" (map treeify fs) : [Tree "body" $ map treeify ss]

instance Treeify DerivDecl where
  treeify (RelDecl _ _ i) = Tree "RelDecl" [treeify i]
  treeify DeriveDDecl{} = error "unreachable"
  treeify InvClaus{} = error "unreachable"
  

instance Treeify Imp.Statement where
  treeify (Imp.Decl vd (Just e)) = Tree "Decl" [treeify vd, treeify e]
  treeify (Imp.Decl vd Nothing) = Tree "Decl" [treeify vd]
  treeify (Imp.Assgn l e) = Tree "Assign" [Tree ("lhs_" ++ l) [], Tree "rhs" [treeify e]]
  treeify (Imp.For vs (Just i) arrs (Imp.Seq ss)) = Tree "For" 
    [
      Tree "vs" $ map treeify vs,
      Tree ("iter_" ++ i) [],
      Tree "arrs" $ map treeify arrs,
      Tree "body" $ map treeify ss
    ]
  treeify (Imp.For vs Nothing arrs (Imp.Seq ss)) = Tree "For"
    [
      Tree "vs" $ map treeify vs,
      Tree "arrs" $ map treeify arrs,
      Tree "body" $ map treeify ss
    ]
  treeify (Imp.Cond c (Imp.Seq st) (Imp.Seq sf)) = Tree "ITE" 
    [
      Tree "Expr" [treeify c],
      Tree "BT" $ map treeify st,
      Tree "BF" $ map treeify sf
    ]
  treeify Imp.While{} = error "todo"

instance Treeify Imp.Expr where
  treeify (Imp.Index l r) = Tree "index" [Tree "lhs" [treeify l], Tree "rhs" [treeify r]]
  treeify (Imp.UnOp op i) = Tree ("Unop" ++ show op) [treeify i]
  treeify (Imp.BinOp op l r) = Tree ("Bop"++ show op) [treeify l, treeify r]
  treeify (Imp.VConst var) = Tree ("Var"++var) []
  treeify (Imp.IConst x) = Tree ("IConst" ++ show x) []
  treeify (Imp.BConst b) = Tree ("BConst" ++ show b) []
  treeify Imp.AConst{} = error "todo"
  treeify Imp.App{} = error "todo"

instance Treeify Imp.VDecl where
  treeify (v,t) = Tree "VDecl" [Tree v [], treeify t]
 
instance Treeify Imp.Type where
  treeify (Imp.ArrTy i) = Tree "ArrTy" [treeify i]
  treeify Imp.BoolTy = Tree "BoolTy" []
  treeify Imp.IntTy = Tree "IntTy" []

instance Treeify RelExpr where
  treeify (Foreach ls (Just s) as b) = Tree "Foreach" 
    [
      Tree "vs" $ map (\s -> Tree s []) ls,
      Tree ("iter_" ++ s) [],
      Tree "arrs" $ map (\s -> Tree s []) as,
      Tree "body" [treeify b]
    ]
  treeify (Foreach ls Nothing as b) = Tree "Foreach"
    [
      Tree "vs" $ map (\s -> Tree s []) ls,
      Tree "arrs" $ map (\s -> Tree s []) as,
      Tree "body" [treeify b]
    ]
  treeify (Prev s i) = Tree "Prev" [Tree s [], treeify i]
  treeify (RelUnop o i) = Tree ("RUnop" ++ show o) [treeify i]
  treeify (RelBinop o l r ) = Tree ("RBop" ++ show o) [treeify l, treeify r]
  -- treeify (RelIndex l r ) = treeify l + treeify r + 1
  treeify (RelVar v) = Tree ("RVar" ++ v) []
  treeify (RelInt x) = Tree ("RInt" ++ show x) []
  treeify (RelBool b) = Tree ("RBool" ++ show b) []

implSize (_,main) = size main
specSize (others,_) = size others