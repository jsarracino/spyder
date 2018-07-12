{-# LANGUAGE LambdaCase #-}
module Language.Spyder.Synth.Template (
    genHole
  , addHole
  , fillHoles
  , substInHole
  , splitBlock
  , parseHole
  , parseFixes
  , genHole
  , genStart
  , genEnd
  , rebuildBlock
  , deleteEnd
) where

import Prelude hiding (concat, foldl, elem)

import Language.Spyder.Util

import Language.Boogie.AST      
import qualified Language.Boogie.Position as Pos      

import qualified Data.Map.Strict as Map
import Data.Foldable
import Data.Maybe
import Data.Functor

buildPred p s = Predicate [Attribute p [SAttr s]] $ SpecClause Inline False $ Pos.gen tt 

genHole :: String -> LStatement
genHole name = Pos.gen ([], Pos.gen $ buildPred "cegis_hole" name)

genStart :: String -> LStatement
genStart name = Pos.gen ([], Pos.gen $ buildPred "cegis_hole_begin" name)

genEnd :: String -> LStatement
genEnd name = Pos.gen ([], Pos.gen $ buildPred "cegis_hole_end" name)
 

addHole :: String -> Block -> Block
addHole s b = b ++ [genHole s]

-- assumes all fixes are in the top level, and do not overlap
parseFixes :: [String] -> Block -> Map.Map String Block
parseFixes names b = Map.fromList $ zip names (map worker names)
  where
    worker :: String -> Block
    worker s = let (h, md, _) = splitBlock (pref s) (suf s) b in last h : md
    pref s (Pos.Pos _ (_, Pos.Pos _ (Predicate [Attribute i args] _ ))) = i == "cegis_hole_begin" && SAttr s `elem` args
    pref _ _  = False
    suf s (Pos.Pos _ (_, Pos.Pos _ (Predicate [Attribute i args] _ ))) = i == "cegis_hole_end" && SAttr s `elem` args
    suf _ _ = False
    
  -- snd $ worker (b, Map.empty) 
  -- where 
  --   worker :: (Block, Map.Map String Block) -> (Block, Map.Map String Block)
  --   worker ([], env) = ([], env)
  --   worker (b, env) = worker (b', env')
  --     where
  --       (nextPref, nextHole, b') = parseHole b
  --       env' = case nextPref of 
  --         (Pos.Pos _ (_, Pos.Pos _ (Predicate [Attribute "cegis_hole_begin" (SAttr s:_)] _)):_) -> Map.insert s nextHole env
  --         _ -> env


rebuildBlock :: Block -> Map.Map String Block -> Block
rebuildBlock = Map.foldlWithKey substInHole 

deleteEnd :: String -> Block -> Block
deleteEnd it blk = worker ([], reverse blk)
  where
    worker (x, []) = x
    worker (l, x:xs) =
      case x of 
        (Pos.Pos _ (_, Pos.Pos _ (Predicate [Attribute "cegis_hole_end" args] _))) -> if SAttr it `elem` args then reverse xs ++ l else worker (x:l, xs)
        _ -> worker (x:l, xs)
substInHole :: Block -> String -> Block -> Block
substInHole blk v subst = if null md then blk >>= substStmt else pref ++  genHole v:subst ++ genEnd v:suf
  where
    substStmt :: LStatement -> [LStatement]
    substStmt (Pos.Pos x (ls, Pos.Pos z (While e c b))) = [Pos.Pos x (ls, Pos.Pos z $ While e c $ recur b)]
    substStmt (Pos.Pos x (ls, Pos.Pos z (If e t f))) = [Pos.Pos x (ls, Pos.Pos z $ If e (recur t) $ fmap recur f)]
    substStmt s = [s]
    
    (pref, md, suf) = splitBlock takePref takeSuf blk
    takePref (Pos.Pos _ (_, Pos.Pos _ (Predicate [Attribute s args] _ ))) = s == "cegis_hole" && SAttr v `elem` args
    takePref s = False
    takeSuf (Pos.Pos _ (_, Pos.Pos _ (Predicate [Attribute s args] _ ))) = s == "cegis_hole_end" && SAttr v `elem` args
    takeSuf s = False

    recur b = substInHole b v subst

splitBlock :: (LStatement -> Bool) -> (LStatement -> Bool) -> Block -> (Block, Block, Block)
splitBlock begin end splitme = (pref, mid, suf)
  where
    (pref, rst) = incBreak begin splitme
    (mid, suf) = incBreak end rst


takePred :: (String -> Bool) -> LStatement -> Bool
takePred p (Pos.Pos _ (_, Pos.Pos _ (Predicate [Attribute s _] _ ))) = p s 
takePred _ _ = False 

parseHole :: Block -> (Block, Block, Block)
parseHole = splitBlock prefP suffP 
  where
    prefP = takePred (== "cegis_hole_begin")
    suffP = takePred (== "cegis_hole_end")

fillHoles :: Body -> Block -> (Block, Body, Map.Map String [String])
fillHoles scope template = (finalTemp, finalScope, tmps)
  where
    vname = "cegis_hole_temp"
    
    worker :: (Block, Body, Map.Map String [String]) -> LStatement -> (Block, Body, Map.Map String [String])
    worker (acc, scp, env) (Pos.Pos o (l, Pos.Pos i (Predicate [Attribute "cegis_hole" [SAttr name]] c))) = 
      (acc ++ [genStart tmp, newS, genEnd tmp], scp', Map.insertWith (++) name [tmp] env)
      where
        (tmp, scp') = allocInBlock vname IntType scp
        newS = Pos.Pos o (l, Pos.Pos i (Predicate [Attribute "cegis_hole" [SAttr name, SAttr tmp]] c))
    worker (acc, scp, env) (Pos.Pos o (l, Pos.Pos i (If c tr fl))) = 
      (acc ++ [newS], scp'', env'')
      where
        newS = Pos.Pos o (l, Pos.Pos i $ If c tr' fl')
        (tr', scp', env') = foldl worker ([], scp, env) tr
        (fl', scp'', env'') = case fl of 
          Just b  -> let (b', s, e) = foldl worker ([], scp', env') b in (Just b', s, e)
          Nothing -> (Nothing, scp', env')
    worker (acc, scp, env) (Pos.Pos o (l, Pos.Pos i (While c inv bod))) = 
      (acc ++ [newS], scp', env')
      where
        newS = Pos.Pos o (l, Pos.Pos i $ While c inv bod')
        (bod', scp', env') = foldl worker ([], scp, env) bod
    worker (acc, scp, env) s = (acc ++ [s], scp, env)

    init = ([], scope, Map.empty)

    (finalTemp, finalScope, tmps) = foldl worker init template

  
