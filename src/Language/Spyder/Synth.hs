module Language.Spyder.Synth (
    fixProc
  , fixBlock
  , fixStmt
  , parseLoop
  , parseLoopInfo
  , findEdited
  , findUnedited
  , rebuildBlock
  , parseFixes
  , dimzero
  , insertIntoLoop
  , buildConds
) where

import Prelude hiding (foldl, all, concat, any)
import Data.Foldable                


import Language.Spyder.Synth.Verify
import Language.Spyder.Synth.Template 
import Language.Spyder.Synth.Enum
import Language.Spyder.Synth.Cegis            (repairBlock, buildMain)
import qualified Language.Spyder.AST as SAST
import qualified Language.Spyder.AST.Spec as Spec               
import Language.Spyder.AST.Component          (Component(..), MainDecl(..))
import qualified Language.Spyder.AST.Imp  as Imp
import Language.Spyder.Synth.Context          (buildContext)
import Language.Boogie.AST                    
import qualified Data.Map.Strict as Map       
import qualified Data.Set as Set
import Language.Boogie.Position               (node, Pos(..), gen)
import Data.List                              (delete, nub, intersect, (\\), partition)
-- import qualified Data.Foldable  as FLD

import Data.Maybe
import Language.Spyder.Opt 

import Language.Spyder.Translate.Expr
import Language.Spyder.Translate.Specs
import Language.Spyder.Translate.Direct

import Language.Spyder.Translate.Related
import Language.Spyder.Translate.Rename

import Language.Spyder.Util

-- given a program and a basic block *to fix*, run cegis to search for the fix.
fixProc :: DimEnv -> [Spec.RelExpr] -> [Set.Set String] -> Program -> Body -> [String] -> SAST.Program -> Block -> (Block, Program, Body)
fixProc dims invs relVars header body globals p@(comps, MainComp decs) broken = fixed
  where
    fixed = fixBlock dims invs relVars header globals rhsVars body [] broken
    rhsVars = findInScope header body invs

useVars :: [Spec.RelExpr] -> Set.Set String -> [Spec.RelExpr]
useVars invs vs = filter (\v -> (`Set.member` vs) `any` gatherVars [v]) invs


data RequiredComp = Loop | Base | Indet | Mixed deriving (Eq, Show, Ord)

fixForInvs :: [String] -> DimEnv -> RequiredComp
fixForInvs invs dims = foldl worker Indet $ map neededSynth invs
  where
    worker Indet x = x
    worker Loop Loop = Loop
    worker Base Base = Base
    worker Loop Base = Mixed
    worker Base Loop = Mixed
    worker Mixed _ = Mixed

    neededSynth s = case (Map.!) dims s of 
      0         -> Base
      otherwise -> Loop




insertIntoLoop :: Block -> Maybe LStatement -> Block
insertIntoLoop ins (Just (Pos p (x, Pos q (While c s bod)))) = [Pos p (x, Pos q $ While c s bod')]
  where
    (pre, mid, suf) = parseLoop bod
    (midp, midl) = (init mid, last mid)
    newMid = case midl of 
      (s@(Pos _ (_, Pos _ While{}))) -> insertIntoLoop ins (Just s)
      _ -> ins 
    bod' = pre ++ mid ++ newMid ++ suf
insertIntoLoop ins (Just s@Pos{}) = s : ins
insertIntoLoop ins _ = ins

dimzero :: DimEnv -> String -> Bool
dimzero d s = (Map.!) d s == 0

fixBlock :: DimEnv -> [Spec.RelExpr] -> [Set.Set String] -> Program -> [String] -> [String] -> Body -> Block -> Block -> (Block, Program, Body)
fixBlock dims invs relVars header globals rhsVars scope prefix fixme = fixResult inner
  where
    genSkel s = [genStart s, genHole s, genEnd s]
    inner = foldl wrapFixStmt initState fixme
    fixResult :: (Block, Block, Program, Body) -> (Block, Program, Body)
    fixResult (_, blk, prog@(Program decs), scope'@(vs, _)) = if not isRepaired then fixed else (blk, prog, scope')
      where
        
        compInvs = map (specToBoogie []) invs -- (filter (isRelated relVars rhsVars) invs)
        isRepaired = checkProg $ optimize $ Program $ decs ++ [buildMain compInvs globals (vs, blk)]
        

        staleInvs = useVars invs $ Set.fromList $ findEdited blk
        staleVars = findUnedited relVars rhsVars blk
        -- (invs', builder) = transRels staleInvs

        

        (suffix, prog', bod') = createFix dims invs prog globals staleVars (filter (dimzero dims) rhsVars) scope' blk 

        fixed = (blk ++ suffix, prog', bod')

    initState = (prefix, [], header, scope)
    wrapFixStmt :: (Block, Block, Program, Body) -> LStatement -> (Block, Block, Program, Body)
    wrapFixStmt (pref, blk, prog, bod) (Pos o (ls, Pos i s@While{})) = (pref', blk'', prog'', bod')
      where
        (s', prog'', bod') = fixStmt dims invs relVars globals rhsVars (pref, prog', scope') s
        prefix = case s' of 
          x@While{} -> map buildLoopAssm invs -- TODO: this doesn't actually work because the dims are wrong. see check.bpl for details.
          _         -> [] 
        (blk', prog', scope') = fixBlock dims invs relVars prog globals rhsVars bod pref blk
        blk'' = blk' ++ prefix ++ [Pos o (ls, Pos i s')]

        pref' = blk''
    wrapFixStmt (pref, blk, prog, bod) (Pos o (ls, Pos i s)) = (pref', blk', prog', bod')
      where
        (s', prog', bod') = fixStmt dims invs relVars globals rhsVars (pref, prog, bod) s
        prefix = case s' of 
          x@While{} -> map buildLoopAssm invs -- TODO: this doesn't actually work because the dims are wrong. see check.bpl for details.
          _         -> [] 
        fix = prefix ++ [Pos o (ls, Pos i s')]
        blk' = blk ++ fix
        pref' = pref ++ fix

    buildLS s = gen ([], gen s)

    createFix :: DimEnv -> [Spec.RelExpr] -> Program -> [String] -> [String] -> [String] -> Body -> Block -> (Block, Program, Body)
    createFix dims invs prog globals lvars rvars scope oldblk = ( rebuildBlock skeleton fixes, finalProg, finalScope)
      where
        initState = (dims, invs, scope, templ, Set.fromList lvars, Set.fromList rvars)
        canSynth (ds, _, _, _, lvs, _) = all (dimzero ds) lvs

        worker (odims, oinvs, oscope, blk, lvs, rvs) = (dims', invs', scope', blk', lvs', rvs')
          where 
            lvs' = okvs `Set.union` Set.fromList loopVars
            rvs' = rvs `Set.union`  (Set.filter (dimzero dims') $ Set.fromList (idx : loopVars))
            (okvs, arrvs) = Set.partition (dimzero odims) lvs

            loopDecs = map genLoopV $ Set.toList arrvs

            genLoopV arrV = ("cegis_loop_var", lty)
              where
                lty = case buildTy $ (Map.!) odims arrV of 
                  (Imp.ArrTy i) -> i
                  _         -> error "inconceivable"

            (itws, r) = oscope

            (loopSS, newItws) = translateStmt (Imp.For loopDecs (Just "cegis_loop_idx") (map Imp.VConst $ Set.toList arrvs) (Imp.Seq []), concat itws)
            scope' = ([newItws], r)

            (oldPre, oldLoop) = (init blk, last blk)

  

            blk' = if null blk then loopBLK else oldPre ++ insertIntoLoop loopBLK (Just oldLoop)

            (newLoopPre, While c spec bod) = (init loopSS, last loopSS)
            loopSS' = newLoopPre ++ [While c spec' bod']
            loopBLK = map (\s -> gen ([], gen s)) loopSS' ++ holes

            holes = filter (dimzero odims) loopVars >>= genSkel

            (idx, loopVars, arrVs) = parseLoopInfo $ head bod
            (pref, mid, suf) = parseLoop bod
    
            --(relInvs, unrelInvs) = partition (isRelated relVars rhsVars) invs
            invs' = map (specializeSpec loopVars arrVs idx) invs 
    
            spec' = spec ++ map buildLoopInv invs
            bod' = pref ++ mid ++ suf ++ map buildLoopAssm invs'
    
            dims' = addDims odims loopDecs `Map.union` Map.singleton idx 0


        (finalDims, finalInvs, synthScope, skeleton, lvs, rvs) = until canSynth worker initState
        templ = buildConds (concat [[genStart s, genHole s, genEnd s] | s <- Set.toList lvs]) finalInvs
        

        (fixed, finalProg, finalScope) = repairBlock (map (specToBoogie []) finalInvs) prog globals (Set.toList lvs) (Set.toList rvs) synthScope oldblk templ

        fixes = parseFixes fixed
  
      
        
      
      -- case fixForInvs lvars dims of 
      -- Base -> repairBlock (map (specToBoogie []) invs') prog globals lvars' rvars' scope blk builder
      -- Loop -> (map buildLS $ loopPre ++ [While c spec' bod'], finalProg, finalScope) -- let specInvs = specializeSpec
      -- Indet -> error "inconceivable"
      -- Mixed -> error "unsatisfiable invariants in program"
      -- where

      --   lvars' = filter (\s -> (Map.!) dims s == 0) lvars
      --   rvars' = filter (\s -> (Map.!) dims s == 0) rvars

      --   (invs', builder) = transRels invs
      --   loopDecs = map worker lvars

        -- (itws, r) = scope

        -- (loopSS, newItws) = translateStmt (Imp.For loopDecs (Just "cegis_loop_idx") (map Imp.VConst lvars) (Imp.Seq []), concat itws)
        -- scope' = ([newItws], r)

        -- (loopPre, While c spec loopBod) = (init loopSS, last loopSS)

        -- (idx, loopVars, arrVs) = parseLoopInfo $ head loopBod
        -- (pref, mid, suf) = parseLoop loopBod

        -- --(relInvs, unrelInvs) = partition (isRelated relVars rhsVars) invs
        -- invs'' = map (specializeSpec loopVars arrVs idx) invs' 

        -- spec' = spec ++ map buildLoopInv invs
        -- bod' = pref ++ loopFix ++ suf ++ map buildLoopAssm invs''

        -- (_, builder') = transRels invs''

        -- dims' = addDims dims loopDecs

      --   (loopFix, finalProg, finalScope) = createFix dims' invs'' prog globals loopVars (rvars ++ loopVars) scope' blk

buildConds :: Block -> [Spec.RelExpr] -> Block
buildConds assigns specs = if any isImp canon then foldl worker [] canon else assigns
  where
    canon = until allBase splitAnds specs
    allBase = all (not . isAnd)

    splitAnds ss = ss >>= f
      where 
        f (Spec.RelBinop Spec.And l r) = [l, r]
        f s = [s]

    worker acc (Spec.RelBinop Spec.Imp l _) = acc ++ singletonBlock (gen $ If (Expr $ specToBoogie [] l) assigns Nothing)
    worker acc _ = acc

    isImp (Spec.RelBinop Spec.Imp _ _) = True
    isImp _ = False
    isAnd (Spec.RelBinop Spec.And _ _) = True
    isAnd _ = False

-- specialize ForEach vs arrs idx bod[vs] to bod[x/v | x <- xs, v <- vs, v <= arr, arrs ~ xs by arr = xs_i]
-- loopArrs should be strictly bigger than arrs. loopArrs is all arrays that might be related to the source
-- array, which includes arrs. 
specializeSpec :: [String] -> [String] -> String -> Spec.RelExpr -> Spec.RelExpr
specializeSpec loopVars loopArrs loopIdx (Spec.Foreach vs relIdx arrs bod) = bod'
  where
    bod' = alphaRel (names `Map.union` idxBind) bod
    idxBind = Map.fromList $ maybeToList relIdx `zip` [Spec.RelVar loopIdx]
    loopBinds = Map.fromList $ loopArrs `zip` loopVars
    foreachBinds = Map.fromList $ arrs `zip` vs
    names :: Map.Map String Spec.RelExpr
    names = Map.foldlWithKey buildTup Map.empty foreachBinds

    buildTup :: Map.Map String Spec.RelExpr -> String -> String -> Map.Map String Spec.RelExpr
    buildTup mp arr arrv = case Map.lookup arr loopBinds of
      Just loopv -> Map.insert arrv (Spec.RelVar loopv) mp
      Nothing -> mp -- error "inconceivable"?

    
specializeSpec _ _ _ x = x

fixStmt :: DimEnv -> [Spec.RelExpr] -> [Set.Set String] -> [String] -> [String] -> (Block, Program, Body) -> BareStatement -> (BareStatement, Program, Body)
fixStmt dims invs relVars globals rhsVars (prefix, prog, scope) = worker
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
        (fixed, prog', scope') = fixBlock dims invs' relVars' prog globals' rhsVars' scope prefix mid

        --(relInvs, unrelInvs) = partition (isRelated relVars rhsVars) invs
        invs' = map (specializeSpec vs arrs idx) invs -- relInvs ++ unrelInvs
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
    recur p b s = fixBlock dims invs relVars p globals rhsVars b prefix s


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
    recurBStmt edits (Assign assns _) = edits ++ map fst assns
    recurBStmt edits _ = edits


findUnedited :: [Set.Set String] -> [String] -> Block -> [String]
findUnedited rels globals blk = (computeRels used rels) \\ used
  where
    used = findEdited blk

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
    recur (Spec.Foreach vs idx arrs i) = vs ++ arrs ++ maybeToList idx ++ recur i
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
parseLoop = splitBlock (takePred "forBegin") (takePred "forEnd")
  where
    takePred :: String -> LStatement -> Bool
    takePred s (Pos _ (_, Pos _ (Predicate [Attribute s' _] _ ))) = s == s'
    takePred _ _ = False