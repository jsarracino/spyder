module Language.Spyder.AST.Spec (
    InvDecl(..)
  , Program
) where
import qualified Language.Spyder.AST.Imp as Imp

data InvDecl =
    BaseInv Imp.Expr          -- a simple invariant that should hold over variables
  | ArrInv Imp.Expr Imp.Expr Imp.Expr -- foo ~ bar | foo = bar ==== elementwise equality between foo and bar
  deriving (Eq, Show, Ord)

type Program = ([InvDecl], Imp.Statement)
