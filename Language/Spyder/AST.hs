module Language.Spyder.AST (
    module Imp
  , module Spec
  , Program(..)
  , Procedure(..)
) where

import qualified Language.Spyder.AST.Imp as Imp
import qualified Language.Spyder.AST.Spec as Spec

data Procedure = Proc {name :: String, formals :: [Imp.VDecl], body :: Imp.Block} deriving (Eq, Show)

data Program = Program {vars :: [Imp.VDecl], invs :: [Spec.RelExpr], procs :: [Procedure]} deriving (Eq, Show)
