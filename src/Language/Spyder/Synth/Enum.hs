{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Language.Spyder.Synth.Enum (
  RhsExpr(..)
  -- , enumExprs
  , grow
  , enumManyExprs
  , enumManyExprs'
  , enumLevel
  , enum
  , translate
  , expand
  , bases
  , Enumerable
  , Translateable
) where

import qualified Language.Spyder.Synth.Context as Ctx


import qualified Language.Spyder.AST.Imp as SAST
import Data.List
import Control.Monad                           (join)
import Language.Spyder.Util


class Enumerable f where
  expand :: Ctx.Context -> [f] -> [f]
  bases :: Ctx.Context -> [f]

class Translateable l r where
  translate :: Ctx.Context -> l -> [r]

enum :: Enumerable f => Ord f => Ctx.Context -> [f]
enum ctx =  join $ iterate (grow ctx) (bases ctx)

grow :: Enumerable f => Ord f => Ctx.Context -> [f] -> [f]
grow ctx es = expand ctx es ++ bases ctx
-- grow = expand
-- -- grammar for statements. 
-- data StmtGram = 
--   Assgn String 


-- grammar for rhs expressions. concrete values will be instantiated later. 
data RhsExpr =
    RVConst | IConst 
  -- | BConst Boolean
  | BinOp RhsExpr RhsExpr
  | UnOp RhsExpr
  deriving (Eq, Show, Ord)

isInt :: RhsExpr -> Bool
-- isInt BConst{} = False
isInt (BinOp l r) = isInt l && isInt r
isInt (UnOp i) = isInt i
isInt _ = True

isVar :: RhsExpr -> Bool
isVar RVConst{} = True
isVar _ = False

instance Enumerable RhsExpr where
  expand ctx es =
    [BinOp l r | l <- es, isInt l, r <- es, isInt r] ++
    [UnOp v | v <- es, isInt v]
  bases _ = [IConst, RVConst]

instance Translateable RhsExpr SAST.Expr where
  translate (Ctx.SynthCtx vars _) RVConst = [ SAST.VConst v | v <- vars]
  translate (Ctx.SynthCtx _ ints) IConst = [ SAST.IConst v | v <- ints]
  translate ctx@(Ctx.SynthCtx vars ints) (BinOp l r) = filter checker $ fmap worker [(o, a) | o <- ops, a <- res]
    where
      worker (o,(a,b)) = SAST.BinOp o a b
      ops = [SAST.Plus, SAST.Minus, SAST.Mul, SAST.Lt, SAST.Gt, SAST.Le, SAST.Ge , SAST.And, SAST.Or, SAST.Eq, SAST.Neq] -- ++[SAST.Div]
      res = [(x,y) | x <- recur l, y <- recur r]
      recur = translate ctx
      -- checker e = case SAST.typecheck e of
      --   (Just (SAST.BaseTy "int")) -> True
      --   _                          -> False
      checker _ = True
  translate ctx@(Ctx.SynthCtx vars ints) (UnOp i) = filter checker $ [(o, i) | o <- ops] >>= worker
    where
      worker (o,i) = fmap (SAST.UnOp o) (translate ctx i)
      ops =  [ SAST.Neg, SAST.Not]
      -- checker e = case SAST.typecheck e of
      --   (Just (SAST.BaseTy "int")) -> True
      --   _                          -> False
      checker _ = True

-- grammar for lhs expressions. for now, just variables.
data LhsExpr = 
    LVConst
  deriving (Eq, Show, Ord)

instance Translateable LhsExpr SAST.Expr where
  translate (Ctx.SynthCtx vars _) LVConst = [ SAST.VConst v | v <- vars]

-- grammar for statements. sequence and assign.
data StmtGram = 
    Assgn LhsExpr RhsExpr
  | Seq [StmtGram]
  deriving (Eq, Show, Ord)






-- given a list of base vars and base ints, return a list of possible exprs. each entry in the
-- list is an infinite list of possiblities, in depth-first order.
-- TODO: give a real name to these
enumManyExprs :: [([SAST.Expr], [SAST.Expr])] -> [[[SAST.Expr]]]
enumManyExprs bases = undefined "REMOVE"

  

enumManyExprs' :: [([SAST.Expr], [SAST.Expr])] -> [[[SAST.Expr]]]
enumManyExprs' bases = map enumLevel (allLevels $ enumManyExprs bases)

-- given a list of exprs [[[TSC.Expr]]] enumerate everything in one level
-- e.g. [[
    --    [0,1], [0,1,0 op 1]
    -- ...], [
    --    [1,2,3], [1,2,3,1+1,1+...]
    -- ...],
    -- ... (finite)
    -- ]
    -- =>

    -- [[0,1][0,2][0,3][1,1],[1,2][1,3],[0,1]]

advanceExprs :: [[[SAST.Expr]]] -> [[[SAST.Expr]]]
advanceExprs = map tail

allLevels :: [[[SAST.Expr]]] -> [[[[SAST.Expr]]]]
allLevels = iterate advanceExprs

enumLevel :: [[[SAST.Expr]]] -> [[SAST.Expr]]
enumLevel ess = breadthFirst $ map head ess

-- allExprs ess = map enumLevel (allLevels ess)


-- enumExprs :: [TSC.Expr] -> [TSC.Expr] -> [TSC.Expr]
-- enumExprs vars ints = concatMap Set.toList $ exprs >>= Set.map (concretize vars ints)
--   where
--     exprs = iterate grow $ Set.fromList [VConst, IConst] -- ;)
