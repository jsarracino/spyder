module Language.Spyder.Translate.Related (
    relatedVars
  , computeRels
  , alphaRels
  , completeLoop
  , dim
  , addDim
  , DimEnv
  , addDims
  , addITWs
  , buildTy
  , allocFreshSpy
  , relatedFromInvs
  , varRelated
  , genDims
) where

import Language.Spyder.AST
import Language.Spyder.AST.Component
import Language.Spyder.AST.Spec
import qualified Language.Spyder.AST.Imp as Imp
import qualified Language.Boogie.AST as BST

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

import Language.Spyder.Util

import Language.Spyder.Translate.Expr

import Data.Maybe
import Data.List

-- given a program, calculate groups of related vars
-- two vars are *related* if there is a transitive data dependency between the two wrt relations.
-- the idea is if one var in a set of related vars is modified, so should the others
relatedVars :: Program -> [Set.Set String]
relatedVars prog = map (\x -> Set.fromList $ gatherVars [x]) (invs prog) 


relatedFromInvs :: [RelExpr] -> [Set.Set String]
relatedFromInvs invs = filter (not . Set.null) $ map (Set.fromList . gatherVars . (: [])) invs

alphaRels :: [Set.Set String] -> Map.Map String String -> [Set.Set String]
alphaRels rels varmap = renamed
  where
    renamed = map (Set.map rename) rels
    rename s = Map.findWithDefault s s varmap

computeRels :: [String] -> [Set.Set String] -> [String]
computeRels vs rels = (Set.toList . snd) $ until transitiveClo growRels init
  where 
    grow :: Set.Set String -> Set.Set String
    grow xs = let nxts = filter (\rel -> not $ Set.null $ rel `Set.intersection` xs) rels in foldl Set.union Set.empty nxts
    growRels :: (Set.Set String, Set.Set String) -> (Set.Set String, Set.Set String)
    growRels (_, n) = (n, grow n)
    transitiveClo :: (Set.Set String, Set.Set String) -> Bool
    transitiveClo (l, r) = l == r
    init :: (Set.Set String, Set.Set String)
    init = (Set.fromList vs, grow $ fst init)

varRelated :: [Set.Set String] -> [String] -> String -> Bool
varRelated rels ls r = r `elem` computeRels ls rels


type DimEnv = Map.Map String Int

buildTy :: Int -> Imp.Type
buildTy n 
  | n == 0    = Imp.IntTy
  | otherwise = Imp.ArrTy $ buildTy (n-1)

dim :: Imp.Type -> Int
dim (Imp.ArrTy i) = 1 + dim i
dim _ = 0

dimBT :: BST.Type -> Int
dimBT (BST.MapType _ _ i) = 1 + dimBT i
dimBT _ = 0

addDim :: DimEnv -> Imp.VDecl -> DimEnv
addDim env (v, ty) = Map.insert v (dim ty) env

addDims :: DimEnv -> [Imp.VDecl] -> DimEnv
addDims = foldl addDim

genDims :: DimEnv -> [BST.IdTypeWhere]
genDims env = map worker $ Map.toList env
  where
    worker (v, i) = translateITW (v, buildTy i)


addITW :: DimEnv -> BST.IdTypeWhere -> DimEnv
addITW env (BST.IdTypeWhere v bt _) = Map.insert v (dimBT bt) env
addITWs :: DimEnv -> [[BST.IdTypeWhere]] -> DimEnv
addITWs env itws = foldl addITW env $ concat itws

allocFreshSpy :: String -> Imp.Type -> DimEnv -> (String, DimEnv)
allocFreshSpy name ty env = (name', Map.insert name' (dim ty) env)
  where
    name' = mk name suffix
    mk pref suf = if suf == 0 then pref else pref ++ show suf
    suffix = worker 0 $ Map.keys env
    worker :: Int -> [String] -> Int
    worker s names = if mk name s `elem` names then worker (s+1) names else s

completeLoop :: [Set.Set String] -> DimEnv -> MainDecl -> MainDecl
completeLoop rels dims (ProcDecl nme formals (Imp.Seq ss)) = ProcDecl nme formals $ Imp.Seq ss'
  where
    (ss', _, _) = foldl worker ([], dims, rels) ss
    worker :: ([Imp.Statement], DimEnv, [Set.Set String]) -> Imp.Statement -> ([Imp.Statement], DimEnv, [Set.Set String])
    worker (acc, dims, rels) (Imp.For vs idx arrs (Imp.Seq bod)) = (acc ++ [Imp.For vs' idx arrs' $ Imp.Seq bod'], finalDims, rels)
      where
        names' = map fst vs'
        arrNames = map takeName arrs
        neededArrs = filter (eqDims dims $ head arrNames) $ computeRels arrNames rels \\ arrNames
        (neededVars, dims'') = foldl buildIter ([], dims') neededArrs
        vs' = vs ++ neededVars
        arrs' = arrs ++ map Imp.VConst neededArrs
        dims' = addDims dims vs

        rels' = rels ++ [Set.fromList names']

        (bod', finalDims, _) = foldl worker ([], dims'', rels') bod

        buildIter :: ([Imp.VDecl], DimEnv) -> String -> ([Imp.VDecl], DimEnv)
        buildIter (vs, env) arrName = ((vname, vty):vs, env')
          where
            arrDim = (Map.!) env arrName
            vty = buildTy $ arrDim - 1
            (vname, env') = allocFreshSpy "loop_var" vty env


    worker (acc, dims, rels) (Imp.Cond c (Imp.Seq l) (Imp.Seq r)) = 
      let (tr, dims', _) = foldl worker ([], dims, rels) l 
          (fl, dims'', _) = foldl worker ([], dims', rels) r in
      (acc ++ [Imp.Cond c (Imp.Seq tr) (Imp.Seq fl)], dims'', rels)
    -- worker (acc, dims, rels) (Imp.While c (Imp.Seq ss)) = 
    --   let (ss', dims', _) = foldl worker ([], dims, rels) ss in 
    --     (acc ++ [Imp.While c $ Imp.Seq ss'], dims', rels)
    worker (acc, d, r) x = (acc ++ [x], d, r)
        
    takeName (Imp.VConst v) = fromMaybe v $ stripPrefix "Main$" v 
    eqDims ds s t = (Map.!) ds s == (Map.!) ds t


completeLoop _ _ x = x


