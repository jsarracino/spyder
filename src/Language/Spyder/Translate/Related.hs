module Language.Spyder.Translate.Related (
    relatedVars
  , computeRels
  , alphaRels
) where

import Language.Spyder.AST
import Language.Spyder.AST.Imp
import Language.Spyder.AST.Spec
import Language.Spyder.AST.Component

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

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