module Language.Spyder.AST.Spec (
  BaseRel(..),
  RelType(..)
) where
import qualified Language.Spyder.AST.Imp as Imp


data BaseRel = 
    Exp Imp.Expr
  | ForEach2 Imp.Expr Imp.Expr BaseRel
  deriving (Eq, Show, Ord)

-- basic types of relations

data RelType = 
    SimpRel -- basic relations
  | FuncRel RelType RelType -- functions
  | TupRel RelType RelType  -- ghost tuples
  deriving (Eq, Show, Ord)



