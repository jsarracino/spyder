module Language.Spyder.Bench (
    boogSize
  , spySize
  , invSize
)

where

import qualified Language.Spyder.AST.Spec as Spec
import Language.Spyder.AST
import qualified Language.Boogie.AST as BST

-- don't use this; instead, diff at command line
boogSize :: BST.Program -> Int
boogSize = error "don't use me"

-- size of spyder ast (main less using + relations in components)
spySize :: Program -> Int
spySize = error "TODO SPYDER AST"

-- count of ast nodes for invariant
invSize :: Program -> Int
invSize = error "TODO INVARIANT AST"