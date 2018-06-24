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
) where

import Language.Spyder.AST
import Language.Spyder.AST.Component
import qualified Language.Spyder.AST.Imp as Imp
import qualified Language.Boogie.AST as BST

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map


import Data.Maybe
import Data.List

-- given a program, calculate groups of related vars
-- two vars are *related* if there is a transitive data dependency between the two wrt relations.
-- the idea is if one var in a set of related vars is modified, so should the others
relatedVars :: Program -> [Set.Set String]
relatedVars (_, MainComp decs) = filter (not . Set.null) $ map worker decs 
  where
    worker (MainUD (_, rhs)) = Set.fromList rhs
    worker _ = Set.empty

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


addITW :: DimEnv -> BST.IdTypeWhere -> DimEnv
addITW env (BST.IdTypeWhere v bt _) = Map.insert v (dimBT bt) env
addITWs :: DimEnv -> [[BST.IdTypeWhere]] -> DimEnv
addITWs env itws = foldl addITW env $ concat itws


completeLoop :: [Set.Set String] -> DimEnv -> MainDecl -> MainDecl
completeLoop rels dims (ProcDecl nme formals (Imp.Seq ss)) = ProcDecl nme formals $ Imp.Seq ss'
  where
    ss' = map (worker dims) ss
    worker :: DimEnv -> Imp.Statement -> Imp.Statement
    worker dims (Imp.For vs idx arrs (Imp.Seq bod)) = Imp.For vs' idx arrs' $ Imp.Seq bod'
      where
        names' = map fst vs'
        arrNames = map takeName arrs
        neededArrs = filter (eqDims dims $ head arrNames) $ computeRels arrNames rels \\ arrNames
        neededVars = map buildIter neededArrs
        vs' = vs ++ neededVars
        arrs' = arrs ++ map Imp.VConst neededArrs
        bod' = map (worker dims') bod
        dims' = addDims dims vs'

        buildIter :: String -> Imp.VDecl
        buildIter arrName = ("loop_var", buildTy arrDim)
          where
            arrDim = (Map.!) dims arrName


    worker dims (Imp.Cond c (Imp.Seq l) (Imp.Seq r)) = Imp.Cond c (Imp.Seq $ map (worker dims) l) (Imp.Seq $ map (worker dims) r)
    worker dims (Imp.While c (Imp.Seq ss)) = Imp.While c $ Imp.Seq (map (worker dims) ss)
    worker _ x = x
        
    takeName (Imp.VConst v) = fromMaybe v $ stripPrefix "Main$" v 
    eqDims ds s t = (Map.!) ds s == (Map.!) ds t


completeLoop _ _ x = x


