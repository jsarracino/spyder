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
  , typecheck
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
  | Cond Expr Block Block
  | While Expr Block                 -- while loops
  deriving (Eq, Show, Ord)



-- complex statements
data Block =
    Seq [Statement]
  deriving (Eq, Show, Ord)

  
class Typeable a where
  typecheck :: a -> Maybe Type
  intTy :: a -> Bool
  intTy x = case typecheck x of 
    (Just (BaseTy "int")) -> True
    _                     -> False

  -- VConst String       -- Variables
  -- | IConst Int      -- Integers
  -- -- | SConst String       -- Strings
  -- | BConst Bool         -- Booleans
  -- | AConst [Expr]       -- Arrays
  -- | BinOp Bop Expr Expr -- Binary operations
  -- | UnOp Uop Expr       -- Unary operations
  -- | Index Expr Expr     -- Array indexing e.g. foo[bar]
  -- | App Expr [Expr]       -- function calls (not procedure calls)
instance Typeable Expr where
  typecheck IConst{} = Just $ BaseTy "int"
  typecheck VConst{} = Just $ BaseTy "int"
  typecheck BConst{} = Just $ BaseTy "bool"
  typecheck (BinOp o l r) = case (typecheck l, typecheck r) of
    (Just lt, Just rt) -> if lt == rt then checkOp lt else Nothing
    _                  -> Nothing
    where 
      checkOp ty
        | o `elem` [Plus, Minus, Mul, Div] && ty == BaseTy "int"      = Just ty
        | o `elem` [Lt, Gt, Le, Ge , And, Or] && ty == BaseTy "bool"  = Just ty
        | o `elem` [Eq, Neq]                                          = Just $ BaseTy "bool"
        | otherwise                                                   = Nothing
  typecheck (UnOp o i) = case (o, typecheck i) of
    (Neg, Just (BaseTy "int"))  -> Just $ BaseTy "bool"
    (Not, Just (BaseTy "bool")) -> Just $ BaseTy "bool"
    _                           -> Nothing
  typecheck _ = Nothing -- TODO: fix varconst hack, app, index, aconst
  