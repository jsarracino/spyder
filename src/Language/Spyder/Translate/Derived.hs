module Language.Spyder.Translate.Derived (
    formals
  , instantiate
) where

import Language.Spyder.AST.Component
import Language.Spyder.AST.Imp            (Expr, VDecl, stripTy)
import qualified Data.Map.Strict as Map
import Language.Boogie.AST                (BareExpression(..), Expression)
import Language.Spyder.AST.Spec           (BaseRel(..))
import Language.Boogie.Position           (Pos(..), attachPos)

type Env = Map.Map String String

formals :: Component -> [VDecl]
formals (DerivComp _ decs) = vdecs
  where 
    vdecs = map toVD vDecls
    vDecls = filter takeDD decs 
    toVD (DeriveDDecl v) = v
    takeDD DeriveDDecl{} = True
    takeDD _ = False
formals (MainComp decs) = undefined "main component can't be used"



fwd :: String -> Env -> String
fwd s = Map.findWithDefault s s

renameDDecl :: Env -> DerivDecl -> DerivDecl
renameDDecl mp (DeriveDDecl (v, t)) = DeriveDDecl (v', t)
  where v' = fwd v mp


instantiate :: [String] -> Component -> Component
instantiate concretes c@(DerivComp nme decs) = DerivComp nme newDecs
  where
    fmals = formals c
    fmals' = map stripTy fmals
    env = Map.fromList $ zip fmals' concretes
    newDecs = map (alphaExpr env) decs

alphaExpr :: Map.Map String String -> DerivDecl -> DerivDecl
alphaExpr env (DeriveDDecl (v, t)) = DeriveDDecl (fwd v env, t)
alphaExpr env (InvClaus i) = InvClaus $ BE (alphaRel env i)
alphaExpr env (RelDecl nme formals body) = RelDecl nme formals body'
  where
    env' = deleteAll env $ map stripTy formals
    deleteAll = foldl (flip Map.delete)
    body' = BE $ alphaRel env' body


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
alphaRel :: Map.Map String String -> BaseRel -> BareExpression
alphaRel env (BE e) = recurBE env e
  where 
    recurBE :: Map.Map String String -> BareExpression -> BareExpression
    recurE :: Map.Map String String -> Expression -> Expression
    recurBE _ v@Literal{} = v
    recurBE mp (Var n) = Var $ fwd n mp
    recurBE mp (UnaryExpression op i) = UnaryExpression op $ recurE mp i
    recurBE mp (BinaryExpression op l r) = BinaryExpression op (recurE mp l) (recurE mp r)
    -- recurBE mp (Logical t r) = ??
    -- recurBE mp (Application n args) = Application (fwd n mp) $ map (recurE mp) args
    -- recurBE mp (MapSelection l args) = recur
    recurE mp = preservePos (recurBE mp)


preservePos :: (a -> a) -> Pos a -> Pos a
preservePos f x = attachPos (position x) (f $ node x)