module Language.Spyder.AST.Spec (
  RelExpr(..),
  RelBop(..),
  RelUop(..)
) where


-- basically, the binary expression grammar from Imp, as well as foreaches. 
-- TODO: figure out how to refactor. 
data RelBop =
    Plus | Minus | Mul | Div
  | Lt | Gt | Le | Ge  | And | Or | Eq | Neq
  | Imp | Iff | Mod
  deriving (Eq, Show, Ord)

data RelUop =
  Neg | Not
  deriving (Eq, Show, Ord)

data RelExpr =
    RelVar String       -- Variables
  | RelInt Int      -- Integers
  | RelBool Bool         -- Booleans
  | RelIndex RelExpr RelExpr -- index
  | RelBinop RelBop RelExpr RelExpr -- Binary operations
  | RelUnop RelUop RelExpr       -- Unary operations
  | RelApp String [RelExpr]       -- function calls (not procedure calls)
  | Foreach [String] (Maybe String) [String] RelExpr  -- foreach (x,y,z) [with idx] in (p, q,s) {<expr>}
  deriving (Eq, Show, Ord)


