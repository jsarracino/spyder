module Language.Spyder.Translate.Rename (
    fwd
  , renameDDecl
  , alphaDDecl
  , alphaRel
  , alphaExpr
  , alphaProc
) where

import Language.Spyder.AST.Component
import Language.Spyder.AST.Imp            (Expr(..), VDecl, stripTy, Statement(..), Block(..))
import Language.Boogie.AST                (BareExpression(..), Expression)
import Language.Spyder.AST.Spec           (RelExpr(..))
import Language.Boogie.Position           (Pos(..), attachPos)

import qualified Data.Map.Strict as Map


type Env = Map.Map String String

fwd :: String -> Env -> String
fwd s = Map.findWithDefault s s

fwdVD :: VDecl -> Env -> VDecl
fwdVD (s, t) env = (fwd s env, t)

alphaProc :: Env -> MainDecl -> MainDecl
alphaProc mp (ProcDecl nme formals body) = ProcDecl nme formals' body'
  where
    formals' = map (`fwdVD` mp) formals
    body' = alphaBlock mp body


alphaBlock :: Env -> Block -> Block    
alphaBlock mp (Seq stmts) = Seq $ map recur stmts
  where 
    recur :: Statement -> Statement
    recur v@Decl{} = v
    recur (Assgn l r) = Assgn (alphaExpr mp l) (alphaExpr mp r)
    recur (While c b) = While (alphaExpr mp c) (alphaBlock mp b)
    recur (Cond c t f) = Cond (alphaExpr mp c) (alphaBlock mp t) (alphaBlock mp f)
    recur (For decs arrs bod) = For decs arrs' bod'
      where 
        arrs' = map (alphaExpr mp) arrs
        bod' = alphaBlock mp bod

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
alphaDDecl env (InvClaus i) = InvClaus (alphaRel (Map.map RelVar env) i)
alphaDDecl env (RelDecl nme formals body) = RelDecl nme formals body'
  where
    env' = Map.map RelVar $ deleteAll env $ map stripTy formals
    deleteAll = foldl (flip Map.delete)
    body' = alphaRel env' body


    -- RelVar String       -- Variables
    -- | RelInt Int      -- Integers
    -- | RelBool Bool         -- Booleans
    -- | RelBinop RelBop RelExpr RelExpr -- Binary operations
    -- | RelUnop RelUop RelExpr       -- Unary operations
    -- | RelApp RelExpr [RelExpr]       -- function calls (not procedure calls)
    -- | Foreach [String] [String] RelExpr  -- foreach (x,y,z) in (p, q,s) {<expr>}
alphaRel :: Map.Map String RelExpr -> RelExpr -> RelExpr
alphaRel mp = recur 
  where 
    recur :: RelExpr -> RelExpr
    recur v@RelInt{} = v
    recur v@RelBool{} = v
    recur x@(RelVar v) = Map.findWithDefault x v mp
    recur (RelUnop op i) = RelUnop op $ recur i
    recur (RelBinop op l r) = RelBinop op (recur l) (recur r)
    recur (RelApp v args) = RelApp (subVar v) $ map recur args
    recur (Foreach vs arrs bod) = Foreach (map subVar vs) (map subVar arrs) $ recur bod

    subVar s = case Map.lookup s mp of 
      Just (RelVar v) -> v
      Just _ -> error "variable substitution into non-variable position"
      Nothing -> s

