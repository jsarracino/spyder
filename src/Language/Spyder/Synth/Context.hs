module Language.Spyder.Synth.Context (
    Context(..)
  , buildContext
) where

import Language.Spyder.AST                    (Program(..))
import Language.Spyder.AST.Spec               
import Language.Spyder.AST.Imp                (Expr(..), Statement(..), Block(..))
import Language.Spyder.AST.Component          (Component(..), DerivDecl(..), MainDecl(..))
import qualified Language.Boogie.AST as BST
import qualified Language.Boogie.Position as Pos
import qualified Data.Map.Strict as Map
import Data.List (nub)
import Data.Maybe
data Context = SynthCtx {
    varNames :: [String]
  , numConsts :: [Int]
} deriving (Eq, Show, Ord)

buildContext :: Program -> Map.Map String String -> Context
buildContext _ _ = SynthCtx names consts
  where
    names = ["Main$x", "Main$y"]
    consts = [1, 3, 7, 10, 0]
-- buildContext (comps, main) varnames = SynthCtx names (nub consts)
--   where 
--     names = map snd $ Map.toList varnames
--     consts = main:comps >>= gatherNumsComp
gatherNumsComp :: Component -> [Int]
gatherNumsComp (DerivComp _ ddecs) = ddecs >>= gatherNumsDDec
gatherNumsComp (MainComp mdecs) = mdecs >>= gatherNumsMDec

gatherNumsDDec :: DerivDecl -> [Int]
gatherNumsDDec (DeriveDDecl _) = []
gatherNumsDDec (RelDecl _ _ bod) = undefined "TODO" --gatherNumsBE bod
gatherNumsDDec (InvClaus bod) = undefined "TODO" --gatherNumsBE bod
-- TODO: arrays
gatherNumsMDec :: MainDecl -> [Int]
gatherNumsMDec (MainDDecl _) = []
gatherNumsMDec (ProcDecl _ _ (Seq stmts)) = stmts >>= gatherNumsStmt 
gatherNumsMDec (MainUD _) = []

gatherNumsStmt :: Statement -> [Int]
gatherNumsStmt (Decl _ (Just e)) = gatherNumsExpr e
gatherNumsStmt (Decl _ Nothing) = []
gatherNumsStmt (Assgn l r) = [VConst l, r] >>= gatherNumsExpr
gatherNumsStmt (For _ _ rhs (Seq bod)) = (rhs >>= gatherNumsExpr) ++ (bod >>= gatherNumsStmt)
gatherNumsStmt (While c (Seq bod)) = gatherNumsExpr c ++ (bod >>= gatherNumsStmt)

gatherNumsExpr :: Expr -> [Int]
gatherNumsExpr = undefined "TODO"

gatherNumsBE :: BST.Expression -> [Int]
gatherNumsBE = worker . Pos.node
  where
    worker (BST.Literal (BST.IntValue v)) = [fromIntegral v]
    worker (BST.Application _ args) = args >>= gatherNumsBE
    worker (BST.MapSelection mp idxs) = mp:idxs >>= gatherNumsBE
    worker (BST.MapUpdate mp idxs rhs) = mp:rhs:idxs >>= gatherNumsBE
    worker (BST.Old i) = gatherNumsBE i
    worker (BST.IfExpr c t e) = [c,t,e] >>= gatherNumsBE
    worker (BST.Coercion e _) = gatherNumsBE e
    worker (BST.UnaryExpression _ i) = gatherNumsBE i
    worker (BST.BinaryExpression _ l r) = [l,r] >>= gatherNumsBE
    worker (BST.Quantified _ _ _ i) = gatherNumsBE i

-- Literal Value |
-- Var Id |                                        -- ^ 'Var' @name@
-- Logical Type Ref |                              -- ^ Logical variable
-- Application Id [Expression] |                   -- ^ 'Application' @f args@
-- MapSelection Expression [Expression] |          -- ^ 'MapSelection' @map indexes@
-- MapUpdate Expression [Expression] Expression |  -- ^ 'MapUpdate' @map indexes rhs@
-- Old Expression |
-- IfExpr Expression Expression Expression |       -- ^ 'IfExpr' @cond eThen eElse@
-- Coercion Expression Type |
-- UnaryExpression UnOp Expression |
-- BinaryExpression BinOp Expression Expression |
-- Quantified QOp [Id] [IdType] Expression    
