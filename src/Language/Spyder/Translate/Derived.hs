module Language.Spyder.Translate.Derived (
    formals
  , instantiate
  , prefixApps
) where

import Language.Spyder.AST.Component
import Language.Spyder.AST.Imp            (VDecl, stripTy)
import qualified Data.Map.Strict as Map
import Language.Boogie.AST                (BareExpression(..), Expression)
import Language.Spyder.AST.Spec           (BaseRel(..))
import Language.Boogie.Position           (Pos(..), attachPos)
import Language.Spyder.Translate.Rename   


formals :: Component -> [VDecl]
formals (DerivComp _ decs) = vdecs
  where 
    vdecs = map toVD vDecls
    vDecls = filter takeDD decs 
    toVD (DeriveDDecl v) = v
    takeDD DeriveDDecl{} = True
    takeDD _ = False
formals (MainComp decs) = undefined "main component can't be used"



instantiate :: [String] -> Component -> Component
instantiate concretes c@(DerivComp nme decs) = DerivComp nme newDecs
  where
    fmals = formals c
    fmals' = map stripTy fmals
    env = Map.fromList $ zip fmals' concretes
    newDecs = map (alphaDDecl env) decs

prefixApps :: String -> Expression -> Expression
prefixApps pref = recurPos
  where 
    recur :: BareExpression -> BareExpression
    recur (Application f args) = Application (pref ++ f) $ map recurPos args
    recur (BinaryExpression o l r) = BinaryExpression o (recurPos l) (recurPos r)
    recur (UnaryExpression o i) = UnaryExpression o (recurPos i)
    recur x@Literal{} = x
    recur x@Var{} = x
    recur _ = undefined "TODO"

    recurPos :: Expression -> Expression
    recurPos e = attachPos (position e) $ recur (node e)

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
    --   Quantified QOp [Id] [IdType] Expression 