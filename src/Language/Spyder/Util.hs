module Language.Spyder.Util (
    strip
  , prefix
  , incSpan
  , incBreak
  , breadthFirst
  , allocFreshLocal
  , preservePos
  , allocInBlock
  , front
) where

-- import Data.List
import Control.Monad (join)
import Language.Boogie.AST
import qualified Language.Boogie.Position as Pos
import qualified Data.Set as Set


--- prefix x y: is x a prefix of y?
prefix :: String -> String -> Bool
prefix (x:xs) (y:ys) = if (x == y) then prefix xs ys else False
prefix [] _ = True

-- strip x y: the suffix of y w.r.t to x, i.e. y with the first x elements removed
strip :: String -> String -> String
strip (x:xs) (y:ys) = if (x == y) then strip xs ys else y:ys
strip [] r = r
strip _ [] = []

-- inclusive span: incSpan p a -> (trus, fls), where trus is all trus + the first element that
-- is false
incSpan :: (a -> Bool) -> [a] -> ([a], [a])
incSpan f inp = (pre ++ l, post)
  where (pre, post') = span f inp
        (l, post) = case post' of
          (x:xs) -> ([x], xs)
          [] -> ([], [])

incBreak :: (a -> Bool) -> [a] -> ([a], [a])
incBreak f = incSpan (not . f)

front :: [a] -> [a]
front xs = take (length xs - 1) xs
 
-- take a list of lists and collect the elements in breadth-first order
-- e.g. [[1,2,3], [5,6]] => [[1,5], [2,5], [3,5], [1,6], [2,6], [3,6]]
breadthFirst :: [[a]] -> [[a]]
breadthFirst = foldl worker [[]]
  where worker acc xs = do {x <- xs; a <- acc; return $ a ++ [x]}
-- let foo xs ys = do {x <- xs; y <- ys; return $ x:y}

preservePos :: (a -> a) -> Pos.Pos a -> Pos.Pos a
preservePos f x = Pos.attachPos (Pos.position x) (f $ Pos.node x)

allocInBlock :: String -> Type -> Body -> (String, Body)
allocInBlock pref ty (itws, b) = (vname, ([vdec] : itws, b))
  where
    vname = fst $ allocFreshLocal pref ty $ concat itws
    vdec  = IdTypeWhere vname ty $ Pos.gen tt
 
-- given a overall scope, a prefix, and a list of variables, allocate a new variable (of int type)
-- return the name of the variable and the new list of variables
allocFreshLocal :: String -> Type -> [IdTypeWhere] -> (String, [IdTypeWhere])
allocFreshLocal prefix ty itws  = (mk prefix suffix, vdec:itws)
  where
    mk pref suf = if suf == 0 then pref else pref ++ show suf -- TODO: hack for loops
    suffix = worker 0 $ Set.fromList locNames
    locNames = map itwId itws
    vdec = IdTypeWhere (mk prefix suffix) ty tru 
    tru = Pos.gen tt

    -- check name clashes with other variables and constants
    worker :: Int -> Set.Set String -> Int
    worker suf names = if mk prefix suf `Set.member` names then worker (suf+1) names else suf


-- inclusiveSpan :: (a -> Bool) -> [a] -> ([a], [a])
-- inclusiveSpan pred l = worker ([], l)
--   where
--     worker (x, []) = (reverse x, [])
--     worker (xs, n:r) = if pred n then worker (n:xs, r) else (reverse $ n:xs, r)