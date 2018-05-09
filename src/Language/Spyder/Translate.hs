{-# LANGUAGE LambdaCase #-}

module Language.Spyder.Translate (
    toBoogie
  , translateExpr
  , translateStmt
  , translateBlock
) where

import Language.Spyder.Translate.Direct

import Language.Spyder.AST                        (Program)
import Language.Spyder.AST.Imp
import Language.Spyder.AST.Spec
import Language.Spyder.AST.Component              (MainDecl(..), DerivDecl(..), Component(..))
import qualified Language.Boogie.AST as BST
import qualified Language.Boogie.Position as Pos
import Language.Spyder.Translate.Desugar
import Language.Spyder.Translate.Main
import qualified Data.Map.Strict as Map
import Language.Spyder.Translate.Derived          (instantiate)
import Data.List                                  (find, partition)
import Language.Spyder.Translate.Rename
import Language.Spyder.Synth.Verify               (checkProgFile)
import Language.Spyder.Synth                      (fixProc)
import Language.Boogie.Pretty                     (pretty)
import System.IO.Unsafe                           (unsafePerformIO)


toBoogie :: Program -> BST.Program
toBoogie prog@(comps, MainComp decls) = outProg 
  where
    globalVars = mangleVars "Main" $ gatherDDecls decls
    varMap = Map.fromList $ zipWith stripTy2 (gatherDDecls decls) globalVars
    stripTy2 (l, _) (r, _) = (l, r)
    vDecls = map translateVDecl globalVars

    comps' = map (processUsing varMap comps) (filter takeUsing decls) 
    relDecls = comps' >>= translateRels

    invs = comps' >>= buildInvs

    

    procs = map (alphaProc varMap) $ filter takeProcs decls

    procDecls = map translateProc procs

    withModifies = map (addModifies $ map stripTy globalVars) procDecls
    withRequires = map (addRequires invs) withModifies
    withContracts = map (addEnsures invs) withRequires
    
    -- attempt to gen the program. if the initial translated program verifies, then we're good to go. otherwise, 
    -- for each procedure that doesn't verify, synthesize a fix.
    boogHeader :: [BST.BareDecl]
    boogHeader = relDecls ++ vDecls

    (okProcs, brokenProcs) = partition (checkBoogie . linkHeader) withContracts
    
    linkHeader :: BST.BareDecl -> BST.Program
    linkHeader pd = BST.Program $ map Pos.gen $ boogHeader ++ [pd]



    finalProcs :: [BST.BareDecl]
    finalProcs = okProcs ++ map repairProc brokenProcs

    allDecs = relDecls ++ vDecls ++ finalProcs 

    outProg = BST.Program (map Pos.gen allDecs)

    -- ProcedureDecl Id [Id] [IdTypeWhere] [IdTypeWhere] [Contract] (Maybe Body)
    repairProc (BST.ProcedureDecl nme tyargs formals rets contract (Just (procVars, bod))) = BST.ProcedureDecl nme tyargs formals rets contract bod'
      where 
        bod' = Just (procVars, fixProc invs (map Pos.gen boogHeader) globals prog varMap bod)
        globals = map stripTy globalVars

    takeUsing MainUD{} = True
    takeUsing _ = False
    takeProcs ProcDecl{} = True
    takeProcs _ = False






buildFun :: [VDecl] -> [Statement] -> BST.Decl
-- buildFun decs bod = Pos.gen $ BST.ProcedureDecl "main" [] [] [] [] $ Just bod'
--   where
--     bod' = ([[translateVDecl v] | v <- decs], translateBlock (Seq bod))
buildFun = undefined "TODO"

translateTy :: Type -> BST.Type
translateTy (BaseTy "int") = BST.IntType
translateTy (BaseTy "bool") = BST.BoolType
translateTy (BaseTy _) = undefined "Error: bad type tag"
-- huh. i think this code, and the index code, don't play well...
-- the index code converts a[x][y] => a[x,y], while this converts
-- int[][] to [int][int]int, which should be indexed like a[x][y]
translateTy (ArrTy inner) = BST.MapType [] [BST.IntType] $ translateTy inner

translateVDecl :: VDecl -> BST.BareDecl
translateVDecl v = BST.VarDecl [translateITW v]

mangleVars :: String -> [VDecl] -> [VDecl]
mangleVars prefix = map worker 
  where worker (nme, ty) = (prefix++"$"++nme, ty)

-- renamed main vars (orig -> new), components, use, returns component instantiated with args
processUsing :: Map.Map String String -> [Component] -> MainDecl -> Component
processUsing vs comps (MainUD (nme, args)) = case usedComp of 
    Just c  -> renamedComp c
    Nothing -> undefined "couldn't find the used component"
  where
    usedComp = find takeNme comps
    takeNme (DerivComp n _) = n == nme
    -- two steps: rename the concrete args using vs, and then rename the component using the new args
    args' = map (vs Map.!) args
    renamedComp = instantiate args'

getRelNames :: Component -> [String]
getRelNames (DerivComp nme decs) = map worker relDecs
  where
    worker (RelDecl n _ _) = n
    worker _ = undefined "inconceivable"
    relDecs = filter takeRD decs
    takeRD RelDecl{} = True
    takeRD _ = False

translateRels :: Component -> [BST.BareDecl]
translateRels (DerivComp nme decs) = map buildRel $ filter takeRD decs
  where
    takeRD RelDecl{} = True
    takeRD _ = False

    -- ProcedureDecl Id [Id] [IdTypeWhere] [IdTypeWhere] [Contract] (Maybe Body) |  -- ^ 'ProcedureDecl' @name type_args formals rets contract body@
translateProc :: MainDecl -> BST.BareDecl
translateProc (ProcDecl nme formals rt body) = BST.ProcedureDecl nme [] formals' [] inv body'
  where
    formals' = map translateITW formals
    (decs, bod) = generateBoogieBlock body
    -- ([[translateVDecl v] | v <- decs], translateBlock (Seq bod))
    body' = Just ([[translateITW v] | v <- decs], translateBlock $ Seq bod)
    inv = []

buildRel :: DerivDecl -> BST.BareDecl
buildRel (RelDecl nme formals bod) = BST.FunctionDecl [] nme [] formals' retTy body
  where
    formals' = map translateFormal formals
    retTy = (Nothing, BST.BoolType )
    body = Just $ buildExpr bod
    buildExpr (BE i) = i
    buildExpr _ = undefined "TODO"
buildRel _ = undefined "TODO"

translateFormal :: VDecl -> BST.FArg
translateFormal (v, t) = (Just v, translateTy t)
translateITW :: VDecl -> BST.IdTypeWhere
translateITW (v, t) = BST.IdTypeWhere v (translateTy t) (Pos.gen BST.tt)


addModifies :: [String] -> BST.BareDecl -> BST.BareDecl
addModifies vars (BST.ProcedureDecl nme tyargs formals rets contract bod) = 
  BST.ProcedureDecl nme tyargs formals rets (contract ++ map buildModify vars) bod
  where
    buildModify s = BST.Modifies False [s]
addModifies _ v@_ = v

addRequires :: [BST.Expression] -> BST.BareDecl -> BST.BareDecl
addRequires invs (BST.ProcedureDecl nme tyargs formals rets contract bod) = 
  BST.ProcedureDecl nme tyargs formals rets (contract ++ map buildReq invs) bod
  where
    buildReq = BST.Requires False
addRequires _ v@_ = v

addEnsures :: [BST.Expression] -> BST.BareDecl -> BST.BareDecl
addEnsures invs (BST.ProcedureDecl nme tyargs formals rets contract bod) = 
  BST.ProcedureDecl nme tyargs formals rets (contract ++ map buildReq invs) bod
  where
    buildReq = BST.Ensures False
addEnsures _ v@_ = v

buildInvs :: Component -> [BST.Expression]
buildInvs (DerivComp _ decs) = map buildExpr alwaysDecs
  where
    takeAlways InvClaus{} = True
    takeAlways _ = False
    buildExpr (InvClaus (BE e)) = e
    alwaysDecs = filter takeAlways decs


-- checkProg :: Program -> Bool
-- checkProg = checkBoogie . toBoogie

checkBoogie :: BST.Program -> Bool
checkBoogie prog = unsafePerformIO $ compileProg >> checkProgFile outp
  where
    outp = "tmp.bpl"
    compileProg = writeFile outp (show $ pretty prog)