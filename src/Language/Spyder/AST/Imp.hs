module Language.Spyder.AST.Imp (
    Statement(..)
  , Expr(..)
  , Bop(..)
  , Uop(..)
) where

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
  | IConst Integer      -- Integers
  | SConst String       -- Strings
  | BConst Bool         -- Booleans
  | AConst [Expr]       -- Arrays
  | BinOp Bop Expr Expr -- Binary operations
  | UnOP Uop Expr       -- Unary operations
  | Index Expr Expr     -- Array indexing e.g. foo[bar]
  deriving (Eq, Show, Ord)


-- types of statements
data Statement =
    Decl String Expr                  -- variable decls
  | Assgn Expr Expr                   -- assignment e.g. x = y
  | Loop [String] [Expr] Statement    -- for-in loops
  | Seq [Statement]                   -- sequences of statements
  | Ensure                            -- ensure tags
  deriving (Eq, Show, Ord)
