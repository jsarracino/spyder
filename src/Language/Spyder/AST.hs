module Language.Spyder.AST (
    module Imp
  , module Spec
  , Program
) where

import qualified Language.Spyder.AST.Imp as Imp
import qualified Language.Spyder.AST.Spec as Spec

type Program = Spec.Program
