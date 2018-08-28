module Language.Spyder.AST.Imp (
    Statement(..)
  , Expr(..)
  , Bop(..)
  , Uop(..)
  , Type(..)
  , Block(..)
  , VDecl
  , stripTy
) where

  
data Bop =
    Plus | Minus | Mul | Div
  | Lt | Gt | Le | Ge  | And | Or | Eq | Neq | Mod
  deriving (Eq, Show, Ord)

-- Numeric negation and boolean negation
data Uop =
  Neg | Not
  deriving (Eq, Show, Ord)

data Expr =
    VConst String       -- Variables
  | IConst Int      -- Integers
  | BConst Bool         -- Booleans
  | AConst [Expr]       -- Arrays
  | BinOp Bop Expr Expr -- Binary operations
  | UnOp Uop Expr       -- Unary operations
  | Index Expr Expr     -- Array indexing e.g. foo[bar]
  | App Expr [Expr]       -- function calls (not procedure calls)

  deriving (Eq, Show, Ord)

  
data Type =
    IntTy
  | BoolTy
  | ArrTy Type
  deriving (Eq, Show, Ord)

type VDecl = (String, Type)
stripTy :: VDecl -> String
stripTy = fst

-- simple statements
data Statement =
    Decl VDecl (Maybe Expr)           -- variable decls
  | Assgn String Expr                   -- assignment e.g. x = y
  | For [VDecl] (Maybe String) [Expr] Block        -- forin loops with optional index capture
  | Cond Expr Block Block
  | While Expr Block                 -- while loops
  deriving (Eq, Show, Ord)



-- complex statements
data Block =
    Seq [Statement]
  deriving (Eq, Show, Ord)

  