module Language.Spyder.AST.Imp (
    Statement(..)
  , Expr(..)
  , Bop(..)
  , Uop(..)
  , Type(..)
  , Block(..)
  , VDecl
  , Type
  , stripTy
) where

import qualified Language.Boogie.AST as BST

-- type Type = BST.Type



data Bop =
    Plus | Minus | Mul | Div
  | Lt | Gt | Le | Ge  | And | Or | Eq | Neq
  deriving (Eq, Show, Ord)

-- TODO: extend uops
-- Numeric negation and boolean negation
data Uop =
  Neg | Not
  deriving (Eq, Show, Ord)

data Expr =
    VConst String       -- Variables
  | IConst Int      -- Integers
  -- | SConst String       -- Strings
  | BConst Bool         -- Booleans
  | AConst [Expr]       -- Arrays
  | BinOp Bop Expr Expr -- Binary operations
  | UnOp Uop Expr       -- Unary operations
  | Index Expr Expr     -- Array indexing e.g. foo[bar]
  | App Expr [Expr]       -- function calls (not procedure calls)

  deriving (Eq, Show, Ord)


data Type =
    BaseTy String
  | ArrTy Type
  | FuncTy Type Type
  | Void
  deriving (Eq, Show, Ord)

type VDecl = (String, Type)
stripTy :: VDecl -> String
stripTy (v,t) = v

-- simple statements
data Statement =
    Decl VDecl (Maybe Expr)           -- variable decls
  | Assgn Expr Expr                   -- assignment e.g. x = y
  | Loop [VDecl] [Expr] Block        -- forin loops
  | While Expr Block                 -- while loops
  deriving (Eq, Show, Ord)



-- complex statements
data Block =
    Seq [Statement]
  deriving (Eq, Show, Ord)

  