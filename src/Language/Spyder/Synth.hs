module Language.Spyder.Synth (
  --   generateProgs
  -- , buildContext
  -- , 
    fixProc
  , fixBlock
  , fixStmt
  , parseLoop
  , parseLoopInfo
) where

import Language.Spyder.Synth.Verify
import Language.Spyder.Synth.Enum
import Language.Spyder.Synth.Cegis            (repairBlock, buildMain)
import qualified Language.Spyder.AST as SAST
import qualified Language.Spyder.AST.Spec as Spec                   
import Language.Spyder.AST.Component          (Component(..), MainDecl(..))
import Language.Spyder.AST.Imp                (VDecl, Type(..))
import Language.Spyder.Synth.Context          (buildContext)
import Language.Boogie.AST                    
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Language.Boogie.Position               (node, Pos(..), gen)
import Data.List                              (delete, nub, intersect, (\\))
import Language.Spyder.Opt 

import Language.Spyder.Translate.Expr
import Language.Spyder.Translate.Specs

import Language.Spyder.Translate.Related
import Language.Spyder.Translate.Rename

-- given a program and a basic block *to fix*, run cegis to search for the fix.
fixProc :: [Spec.RelExpr] -> [Set.Set String] -> Program -> Body -> [String] -> SAST.Program -> Block -> (Block, Program, Body)
fixProc invs relVars header body globals p@(comps, MainComp decs) broken = fixed
  where
    fixed = fixBlock invs relVars header globals rhsVars body [] broken
    rhsVars = findInScope header body invs

fixBlock :: [Spec.RelExpr] -> [Set.Set String] -> Program -> [String] -> [String] -> Body -> Block -> Block -> (Block, Program, Body)
fixBlock invs relVars header globals rhsVars scope prefix fixme = fixResult inner
  where
    inner = foldl wrapFixStmt init fixme
    fixResult :: (Block, Block, Program, Body) -> (Block, Program, Body)
    fixResult (_, blk, prog@(Program decs), scope'@(vs, _)) = if not isRepaired then fixed else (blk, prog, scope')
      where
        compInvs = map (specToBoogie []) invs -- (filter (isRelated relVars rhsVars) invs)
        isRepaired = checkProg $ optimize $ Program $ decs ++ [buildMain compInvs globals (vs, blk)]

        (invs', builder) = transRels invs

        (suffix, prog', bod') = repairBlock (map (specToBoogie []) invs') prog globals (findUnedited relVars rhsVars blk) rhsVars scope' blk builder
        fixed = (blk ++ builder suffix, prog', bod')

    init = (prefix, [], header, scope)
    wrapFixStmt :: (Block, Block, Program, Body) -> LStatement -> (Block, Block, Program, Body)
    wrapFixStmt (pref, blk, prog, bod) (Pos o (ls, Pos i s)) = (pref', blk', prog', bod')
      where
        (s', prog', bod') = fixStmt invs relVars globals rhsVars (pref, prog, bod) s
        prefix = case s' of 
          x@While{} -> map buildLoopAssm invs -- TODO: this doesn't actually work because the dims are wrong. see check.bpl for details.
          _         -> [] 
        fix = prefix ++ [Pos o (ls, Pos i s')]
        blk' = blk ++ fix
        pref' = pref ++ fix

    transRels :: [Spec.RelExpr] -> ([Spec.RelExpr], Block -> Block)
    -- transRels rels = snd $ until done step init
    --   where 
    --     nxt :: ([Spec.RelExpr], Block -> Block)
    --     nxt = let (rs', fs') = unzip $ map simplSpec rels in (rs', foldl (.) id fs')
    --     init :: (([Spec.RelExpr], Block -> Block), ([Spec.RelExpr], Block -> Block))
    --     init = ((rels, id), nxt)
    --     step :: (([Spec.RelExpr], Block -> Block), ([Spec.RelExpr], Block -> Block)) -> (([Spec.RelExpr], Block -> Block), ([Spec.RelExpr], Block -> Block))
    --     step (_, x@(rs, f)) = (x, (rs', f'))
    --       where 
    --         (rs', fs) = unzip $ map simplSpec rs
    --         f' = foldl (.) f fs

    --     done :: (([Spec.RelExpr], Block -> Block), ([Spec.RelExpr], Block -> Block)) -> Bool
    --     done ((rs, _), (rs', _)) = all (uncurry (==)) $ rs `zip` rs'
    transRels rels = (rels, foldl (.) id $ map simplSpec rels)

    simplSpec :: Spec.RelExpr -> Block -> Block
    simplSpec (Spec.RelBinop Spec.Imp l r) b = singletonBlock $ gen $ If (Expr $ specToBoogie [] l) b Nothing
    simplSpec x b = b
-- specialize ForEach vs arrs bod[vs] to bod[x/v | x <- xs, v <- vs, v <= arr, arrs ~ xs by arr = xs_i]
-- loopArrs should be strictly bigger than arrs. loopArrs is all arrays that might be related to the source
-- array, which includes arrs. 
specializeSpec :: [String] -> [String] -> Spec.RelExpr -> Spec.RelExpr
specializeSpec loopVars loopArrs (Spec.Foreach vs arrs bod) = bod'
  where
    bod' = alphaRel names bod
    loopBinds = Map.fromList $ loopArrs `zip` loopVars
    foreachBinds = Map.fromList $ arrs `zip` vs
    names :: Map.Map String Spec.RelExpr
    names = Map.foldlWithKey buildTup Map.empty foreachBinds

    buildTup :: Map.Map String Spec.RelExpr -> String -> String -> Map.Map String Spec.RelExpr
    buildTup mp arr arrv = case Map.lookup arr loopBinds of
      Just loopv -> Map.insert arrv (Spec.RelVar loopv) mp
      Nothing -> mp -- error "inconceivable"?

    
specializeSpec _ _ x = x

fixStmt :: [Spec.RelExpr] -> [Set.Set String] -> [String] -> [String] -> (Block, Program, Body) -> BareStatement -> (BareStatement, Program, Body)
fixStmt invs relVars globals rhsVars (prefix, prog, scope) = worker
   where
    worker (If e tru fls) = (If e tru' fls', prog'', bod'')
      where
        (tru', prog', bod') = recur prog scope tru
        (fls', prog'', bod'') = case fls of 
          Just i  -> let (r, p, b) = recur prog' bod' i in (Just r, p, b)
          Nothing -> (Nothing, prog', bod')
    worker (While c spec bod) = (While c spec' bod', prog', scope')
      where
        (idx, vs, arrs) = parseLoopInfo $ head bod
        (pref, mid, suf) = parseLoop bod
        relVars' = relVars ++ [Set.fromList vs]
        (fixed, prog', scope') = fixBlock invs' relVars' prog globals' rhsVars' scope prefix mid

        --(relInvs, unrelInvs) = partition (isRelated relVars rhsVars) invs
        invs' = map (specializeSpec vs arrs) invs -- relInvs ++ unrelInvs
        globals' = []
        rhsVars' = vs

        spec' = spec ++ map buildLoopInv invs
        bod' = pref ++ fixed ++ suf ++ map buildLoopAssm invs

      --   spec' = (map (SpecClause LoopInvariant False) invs) ++ spec
      --   (tru', prog', bod') = recur prog scope tru
      --   (fls', prog'', bod'') = case fls of 
      --     Just i  -> let (r, p, b) = recur prog' bod' i in (Just r, p, b)
      --     Nothing -> (Nothing, prog', bod')
    worker s = (s, prog, scope)
    recur p b s = fixBlock invs relVars p globals rhsVars b prefix s


buildLoopInv :: Spec.RelExpr -> SpecClause
buildLoopInv re = SpecClause LoopInvariant False $ specToBoogie [] re
buildLoopAssm :: Spec.RelExpr -> LStatement
buildLoopAssm re = gen ([], gen $ Predicate [] $ SpecClause Inline True $ specToBoogie [] re)

isRelated :: [Set.Set String] -> [String] -> Spec.RelExpr -> Bool
isRelated rels vars e = null intersection
  where intersection = gatherVars [e] `intersect` computeRels vars rels 

findEdited :: Block -> [String]
findEdited = foldl recurLS [] -- blk
  where
    recurLS :: [String] -> LStatement -> [String]
    recurLS edits ss = recurBLS edits $ node ss
    recurBLS :: [String] -> BareLStatement -> [String]
    recurBLS edits (_, stmt) = recurStmt edits stmt
    recurStmt :: [String] -> Statement -> [String]
    recurStmt edits s = recurBStmt edits (node s)
    recurBStmt edits (Assign assns _) = map fst assns
    recurBStmt edits _ = edits

findUnedited :: [Set.Set String] -> [String] -> Block -> [String]
findUnedited rels globals blk = (computeRels (foldl recurLS globals blk) rels) \\ used
  where
    used = findEdited blk
    recurLS :: [String] -> LStatement -> [String]
    recurLS edits ss = recurBLS edits $ node ss
    recurBLS :: [String] -> BareLStatement -> [String]
    recurBLS edits (_, stmt) = recurStmt edits stmt
    recurStmt :: [String] -> Statement -> [String]
    recurStmt edits s = recurBStmt edits (node s)
    recurBStmt edits (Assign assns _) = foldl (flip delete) edits $ map fst assns
    recurBStmt edits _ = edits

findInScope :: Program -> Body -> [Spec.RelExpr] -> [String]
findInScope (Program decs) _ desugInvs = (decs >>= (getVars . node)) `intersect` invVars
  where
    getVars (VarDecl itws) = map itwId itws
    getVars _ = []
    invVars = gatherVars desugInvs

gatherVars :: [Spec.RelExpr] -> [String]
gatherVars es = nub $ es >>= recur
  where 
    recur :: Spec.RelExpr -> [String]
    recur (Spec.RelVar x) = [x]
    recur (Spec.RelApp _ es) = nub $ es >>= recur
    recur (Spec.RelBinop _ l r) = recur l ++ recur r
    recur (Spec.RelUnop _ i) = recur i
    recur _ = []
    -- MapSel, Quantified, MapUp TODO
            

-- parses {:forInfo idx vs arrs} into (idx, vs, arrs)
parseLoopInfo :: LStatement -> (String, [String], [String])
parseLoopInfo (Pos _ (_, Pos _ (Predicate [Attribute "forInfo" (SAttr x:args)] _))) = (x, vs, arrs)
  where
    takeSAttr (SAttr s) = s
    args' = map takeSAttr args
    (vs, arrs) = splitAt (length args' `div` 2) args' 

-- parses block with {:forBegin} and {:forEnd} separators into three blocks, one for the prefix, one for suffix, and one for body
parseLoop :: Block -> (Block, Block, Block)
parseLoop ls = (pref, bod, suf)
  where
    (pref, rst) = break (takePred "forBegin") ls
    (bod, suf) = break (takePred "forEnd") rst
    takePred :: String -> LStatement -> Bool
    takePred s (Pos _ (_, Pos _ (Predicate [Attribute s' _] _ ))) = s == s'
    takePred _ _ = False