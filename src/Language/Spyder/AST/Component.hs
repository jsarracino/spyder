module Language.Spyder.AST.Component (
  Component(..)
) where

import Language.Spyder.AST.Imp (Type, VDecl)
import Language.Spyder.AST.Spec (BaseRel, RelType)


newtype InvClaus = InvClaus BaseRel deriving (Eq, Show, Ord)
    


data RDecl = 
    Simp String BaseRel
  | Func String [(String, RelType)] BaseRel
    deriving (Eq, Show, Ord)

data CMemberDecl = 
    DataDecl String Type -- name and type
  | ProcDecl String [VDecl] Type -- name, formals, return type
  | RelDecl RDecl
    deriving (Eq, Show, Ord)

name :: CMemberDecl -> String
name (DataDecl s _) = s
name (ProcDecl s _ _) = s
name (RelDecl (Simp s _)) = s
name (RelDecl (Func s _ _)) = s



-- name, decls, always clauses, uses clauses
data Component = 
    Comp String [CMemberDecl] [InvClaus] [String]
    deriving (Eq, Show, Ord)

