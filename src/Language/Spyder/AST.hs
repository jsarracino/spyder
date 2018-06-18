module Language.Spyder.AST (
    module Imp
  , module Spec
  -- , module Typecheck
  , Program
) where

import qualified Language.Spyder.AST.Imp as Imp
import qualified Language.Spyder.AST.Spec as Spec
-- import qualified Language.Spyder.AST.Typecheck as Typecheck
import Language.Spyder.AST.Component(Component)

type Program = ([Component], Component)
