module Language.Spyder.Translate.Main (
    gatherDDecls
) where

import Language.Spyder.AST.Component    (Component(..), MainDecl(..))
import Language.Spyder.AST.Imp          (VDecl, Type(..))


gatherDDecls :: [MainDecl] -> [VDecl]
gatherDDecls decs = map unwrap $ filter takeDD decs
  where
    takeDD MainDDecl{} = True
    takeDD _ = False
    unwrap (MainDDecl decs) = decs
    unwrap _ = undefined "unexpected argument to unwrap"
-- vars, procs, uses
-- translateMain :: Component -> ([VDecl], [CMemberDecl], [CMemberDecl])
-- translateMain (Comp _ membDecs) = (vars, procs, uses)
--   where
--     vars = foldl (\ctx cd -> processDDecl cd ++ ctx) [] membDecs
--     procs = filter isPDecl membDecs
--     uses = filter isUse membDecs

--     isPDecl ProcDecl{} = True
--     isPDecl _ = False
--     isUse CompUse{} = True
--     isUse _ = False


-- processDDecl :: CMemberDecl -> VDecl
-- processDDecl (DataDecl s ty) = decs
--   where
--     decs = vdec : dimDecs
--     vdec = ("Main$"++s, ty)
--     dimDecs = makeDims ty 0
--     makeDims (BaseTy _) _ = []
--     makeDims (ArrTy i) n = ("Main$"++s++"_dim"++ show n, BaseTy "int") : makeDims i (n+1)

