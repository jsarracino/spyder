module Language.Spyder.AST.Spec (
  BaseRel(..),
  RelType(..),
  injectForeach2
) where
import qualified Language.Boogie.AST as BAST
import Language.Boogie.PrettyAST() -- for show typeclass

-- TODO arrays
data BaseRel = 
    BE BAST.Expression
  | ForEach2 BAST.Expression BAST.Expression BaseRel
  deriving (Eq, Show, Ord)

-- basic types of relations


-- TODO when covering arrays
injectForeach2 :: BAST.Expression -> BaseRel
injectForeach2 = BE
-- data BareExpression = 
--   Literal Value |
--   Var Id |                                        -- ^ 'Var' @name@
--   Logical Type Ref |                              -- ^ Logical variable
--   Application Id [Expression] |                   -- ^ 'Application' @f args@
--   MapSelection Expression [Expression] |          -- ^ 'MapSelection' @map indexes@
--   MapUpdate Expression [Expression] Expression |  -- ^ 'MapUpdate' @map indexes rhs@
--   Old Expression |
--   IfExpr Expression Expression Expression |       -- ^ 'IfExpr' @cond eThen eElse@
--   Coercion Expression Type |
--   UnaryExpression UnOp Expression |
--   BinaryExpression BinOp Expression Expression |
--   Quantified QOp [Id] [IdType] Expression         -- ^ 'Quantified' @qop type_vars bound_vars expr@

data RelType = 
    SimpRel -- basic relations
  | FuncRel RelType RelType -- functions
  | TupRel RelType RelType  -- ghost tuples
  deriving (Eq, Show, Ord)



