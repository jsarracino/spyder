module Language.Spyder.Synth.Schedule (
  scheduleInvs
  , calcEquivClasses
  , scheduleEquivClasses
) where

import Language.Spyder.Synth.Verify
import Language.Spyder.AST.Spec
import Language.Spyder.Util
import qualified Data.Set as Set
import Data.List


-- assumes at least one inv
scheduleInvs :: Set.Set String -> Set.Set String -> [RelExpr] -> [(Set.Set String, [RelExpr])]
scheduleInvs srcs dsts invs = if Set.null dsts then [(srcs, invs)] else ret
  where
    -- 
    ret = case invs of 
      inv:rst -> 
        let vs = gatherVars [inv]
            (detVars, candVars) = partition (`Set.member` srcs) vs
            retVars = Set.fromList candVars
            takeInv i = all (`Set.member` (retVars `Set.union` srcs)) (gatherVars [i])
            (retInvs, rst') = partition takeInv (inv:rst)
            (srcs', dsts') = (srcs `Set.union` retVars, dsts `Set.difference` retVars)
        in
          (retVars, retInvs) : scheduleInvs srcs' dsts' rst'
      _ -> error "expected more invariants in scheduling."
-- scheduleInvs srcs dsts invs = 
--   where
--     classes = calcEquivClasses invs
-- scheduleInvs = error "TODO"
-- scheduleInvs srcs _ invs = scheduleEquivClasses srcs $ calcEquivClasses invs

-- idea: given a bunch of lvars, and a bunch of invs, group the invs into equivalence classes. each equivalence class overlaps
-- with the others by exactly one var (or none at all), and so each group of invs can be solved independently.

-- for example:
-- X = Y, Y = Z overlaps on Y, and so can be solved by 
-- Y := ??; z := ??;

-- X = Y, Y = Z + X must be in the same equiv class (overlap on two vars)

calcEquivClasses :: [RelExpr] -> [[RelExpr]]
calcEquivClasses invs = until canon worker [[i] | i <- invs]
  where
    canon :: [[RelExpr]] -> Bool
    canon classes = all (trivIntersect classes) classes
    overlap :: [RelExpr] -> [RelExpr] -> Bool
    overlap i j = Set.size (Set.fromList (gatherVars i) `Set.intersection` Set.fromList (gatherVars j)) > 1
    trivIntersect :: [[RelExpr]] -> [RelExpr] -> Bool
    trivIntersect cs i = all inner cs
      where
        inner :: [RelExpr] -> Bool
        inner j = i == j || not (overlap i j)
    worker classes = case find (\(i,j) -> i /= j && overlap i j) [(i,j) | i <- classes, j <- classes] of 
      Just (p, q) -> (p ++ q) : (classes \\ [p,q])
      Nothing -> error "inconceivable"

scheduleEquivClasses :: Set.Set String -> [[RelExpr]] -> [(Set.Set String, [RelExpr])]
scheduleEquivClasses _ [] = []
scheduleEquivClasses ss is = ret : scheduleEquivClasses ss' is'
  where
    retClass = case find (\i -> Set.fromList (gatherVars i) `Set.isSubsetOf` ss) is of
      Just i -> i
      _ -> head is 
    ret = (newRetVars, retClass)
    newRetVars = Set.filter (\v -> not $ Set.member v ss) $ Set.fromList $ gatherVars retClass
    ss' = ss `Set.union` newRetVars
    is' = delete retClass is