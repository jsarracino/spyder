module Language.Spyder.Translate.Rename (
    fwd
  , renameDDecl
  , alphaDDecl
  , alphaRel
  , preservePos
  , alphaExpr
  , alphaProc
) where

import Language.Spyder.AST.Component
import Language.Spyder.AST.Imp            (Expr(..), VDecl, stripTy, Statement(..), Block(..))
import Language.Boogie.AST                (BareExpression(..), Expression)
import Language.Spyder.AST.Spec           (BaseRel(..))
import Language.Boogie.Position           (Pos(..), attachPos)

import qualified Data.Map.Strict as Map


type Env = Map.Map String String

fwd :: String -> Env -> String
fwd s = Map.findWithDefault s s

fwdVD :: VDecl -> Env -> VDecl
fwdVD (s, t) env = (fwd s env, t)

alphaProc :: Env -> MainDecl -> MainDecl
alphaProc mp (ProcDecl nme formals rt body) = ProcDecl nme formals' rt body'
  where
    formals' = map (`fwdVD` mp) formals
    body' = alphaBlock mp body

-- TODO: doesn't work for scoping
alphaBlock :: Env -> Block -> Block    
alphaBlock mp (Seq stmts) = Seq $ map recur stmts
  where 
    recur :: Statement -> Statement
    recur v@Decl{} = v
    recur (Assgn l r) = Assgn (alphaExpr mp l) (alphaExpr mp r)
    recur (While c b) = While (alphaExpr mp c) (alphaBlock mp b)
    -- recur (Loop [VDecl] [Expr] Block)

alphaExpr :: Env -> Expr -> Expr
alphaExpr mp = recur
  where
    recur :: Expr -> Expr
    recur (VConst s) = VConst $ fwd s mp
    recur (AConst es) = AConst $ map recur es
    recur (App l r) = App (recur l) (map recur r)
    recur (Index l r) = Index (recur l) (recur r)
    recur (BinOp o l r) = BinOp o (recur l) (recur r)
    recur (UnOp o i) = UnOp o (recur i)
    recur v@IConst{} = v
    recur v@BConst{} = v

renameDDecl :: Env -> DerivDecl -> DerivDecl
renameDDecl mp (DeriveDDecl (v, t)) = DeriveDDecl (v', t)
  where v' = fwd v mp

alphaDDecl :: Map.Map String String -> DerivDecl -> DerivDecl
alphaDDecl env (DeriveDDecl (v, t)) = DeriveDDecl (fwd v env, t)
alphaDDecl env (InvClaus i) = InvClaus $ BE (alphaRel env i)
alphaDDecl env (RelDecl nme formals body) = RelDecl nme formals body'
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
alphaRel :: Map.Map String String -> BaseRel -> Expression
alphaRel env (BE e) = recurE env e
  where 
    recurBE :: Map.Map String String -> BareExpression -> BareExpression
    recurE :: Map.Map String String -> Expression -> Expression
    recurBE _ v@Literal{} = v
    recurBE mp (Var n) = Var $ fwd n mp
    recurBE mp (UnaryExpression op i) = UnaryExpression op $ recurE mp i
    recurBE mp (BinaryExpression op l r) = BinaryExpression op (recurE mp l) (recurE mp r)
    -- recurBE mp (Logical t r) = ??
    recurBE mp (Application n args) = Application (fwd n mp) $ map (recurE mp) args
    -- recurBE mp (MapSelection l args) = recur
    recurE mp = preservePos (recurBE mp)


preservePos :: (a -> a) -> Pos a -> Pos a
preservePos f x = attachPos (position x) (f $ node x)