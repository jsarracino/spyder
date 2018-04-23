module Language.Spyder.Translate.Derived (
    formals
  , instantiate
) where

import Language.Spyder.AST.Component
import Language.Spyder.AST.Imp            (Expr, VDecl, stripTy)
import qualified Data.Map.Strict as Map
import Language.Boogie.AST                (BareExpression(..), Expression)
import Language.Spyder.AST.Spec           (BaseRel(..))
import Language.Boogie.Position           (Pos(..), attachPos)
import Language.Spyder.Translate.Rename   


formals :: Component -> [VDecl]
formals (DerivComp _ decs) = vdecs
  where 
    vdecs = map toVD vDecls
    vDecls = filter takeDD decs 
    toVD (DeriveDDecl v) = v
    takeDD DeriveDDecl{} = True
    takeDD _ = False
formals (MainComp decs) = undefined "main component can't be used"



instantiate :: [String] -> Component -> Component
instantiate concretes c@(DerivComp nme decs) = DerivComp nme newDecs
  where
    fmals = formals c
    fmals' = map stripTy fmals
    env = Map.fromList $ zip fmals' concretes
    newDecs = map (alphaDDecl env) decs

