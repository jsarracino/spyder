module Language.Spyder.AST.Component (
  Component(..),
  CMemberDecl(..),
  RDecl(..)
) where

import Language.Spyder.AST.Imp (Type, VDecl)
import Language.Spyder.AST.Spec (BaseRel, RelType)
    


data RDecl = 
    Simp String BaseRel                     -- simple relation
  | Func String [(String, RelType)] BaseRel -- relation with args
    deriving (Eq, Show, Ord)

    -- decls, always clauses, uses clauses
data CMemberDecl = 
    DataDecl String Type -- name and type
  | ProcDecl String [VDecl] Type -- name, formals, return type
  | RelDecl RDecl        -- relation decl
  | InvClaus BaseRel     -- always clause
  | CompUse String       -- using clause
    deriving (Eq, Show, Ord)

-- name :: CMemberDecl -> String
-- name (DataDecl s _) = s
-- name (ProcDecl s _ _) = s
-- name (RelDecl (Simp s _)) = s
-- name (RelDecl (Func s _ _)) = s



-- name,
data Component = 
    Comp String [CMemberDecl]
    deriving (Eq, Show, Ord)

