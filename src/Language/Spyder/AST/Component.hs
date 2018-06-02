module Language.Spyder.AST.Component (
    Component(..)
  , DerivDecl(..)
  , MainDecl(..)
  , UseClause
) where

import Language.Spyder.AST.Imp (Type, VDecl, Block)
import Language.Spyder.AST.Spec (RelExpr)

type UseClause = (String, [String])

    -- decls, always clauses, uses clauses
data DerivDecl = 
    DeriveDDecl VDecl -- name and type
  | RelDecl String [VDecl] RelExpr        -- relation decl
  | InvClaus RelExpr     -- always clause
    deriving (Eq, Show, Ord)


data MainDecl = 
    MainDDecl VDecl
  | ProcDecl String [VDecl] Type Block  -- name, formals, return type, body
  | MainUD UseClause
    deriving (Eq, Show, Ord)

-- name, list of decls
data Component = 
    MainComp [MainDecl]
  | DerivComp String [DerivDecl]
    deriving (Eq, Show, Ord)


