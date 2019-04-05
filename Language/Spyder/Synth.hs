module Language.Spyder.Synth (
    fixProc
  , fixBlock
  , fixStmt
  , parseLoop
  , parseLoopInfo
  , findEdited
  , findUnedited
  , parseFixes
  , dimzero
  , insertIntoLoop
  , genCegPs
  , log
  , trimCond
  , specializeSpec
  , isRelated
  , fixProcGeneral
  , generatePrevs
  , buildCond
  , compSpecs
  , findEditedDeep
  , stripLoopInfo
  , calculateCegisProblems
  , generateSubProblems
  , combineSynSolutions
  -- , generate
) where

import Prelude hiding (foldl, all, concat, any, concatMap)
import Data.Foldable                


import Language.Spyder.Synth.Verify
import Language.Spyder.Synth.Template 
import Language.Spyder.Synth.Cegis            (repairBlock, buildMain)
import qualified Language.Spyder.AST.Spec as Spec               
import Language.Spyder.AST.Component          (Component(..), MainDecl(..))
import qualified Language.Spyder.AST.Imp  as Imp
import Language.Spyder.Synth.Context          (buildContext)
import Language.Boogie.AST                    
import qualified Data.Map.Strict as Map       
import qualified Data.Set as Set
import Language.Boogie.Position               (node, Pos(..), gen)
import Data.List                              (nub, intersect, (\\), partition)
-- import qualified Data.Foldable  as FLD

import Data.Maybe
import Language.Spyder.Opt 

import Language.Spyder.Translate.Expr
import Language.Spyder.Translate.Specs
import Language.Spyder.Translate.Direct

import Language.Spyder.Translate.Related
import Language.Spyder.Translate.Rebuild
import Language.Spyder.Translate.Rename

import Language.Spyder.Util

import Text.Printf
import System.IO.Unsafe

import Control.Monad (join)

import Language.Spyder.Synth.Schedule

-- given a procedure, fix with general synthesis by searching for a repair at the end
fixProcGeneral :: DimEnv -> [Spec.RelExpr] -> Program -> Body -> [String] -> Block -> (Block, Program, Body)
fixProcGeneral dims invs program scope globals broken = ret
  where
    header = case program of (Program x) -> x
    ret = (broken, Program $ header ++ [generalFunc], scope)
    -- ^ 'ProcedureDecl' @name type_args formals rets contract body@
    generalFunc = gen $ ProcedureDecl "__general_syn" [] [] [] contr $ Just ([], [])
    contr = reqs ++ ensures ++ mods
    compInvs = map (specToBoogie []) invs 
    buildClause :: Bool -> Expression -> LStatement
    buildClause b e = gen ([], gen $ Predicate [] $ SpecClause Inline b e)
    reqs = map (Requires False) compInvs
    ensures = map (Ensures False) compInvs ++ olds
    mods = [Modifies False globals]

    needSaving = findEdited broken `intersect` globals
    olds = map (Ensures False . buildOld) needSaving
    buildOld v = gen $ BinaryExpression Eq (gen $ Old (gen $ Var v)) (gen $ Var v)
    
    



-- given a program and a basic block *to fix*, run cegis to search for the fix.
fixProc :: DimEnv -> [Spec.RelExpr] -> [Set.Set String] -> Program -> Body -> [String] -> Block -> (Block, Program, Body)
fixProc dims invs relVars header body globals broken = fixed
  where
    fixed = fixBlock dims invs relVars header globals rhsVars Set.empty body [] broken
    rhsVars = findInScope header body invs

useVars :: [Spec.RelExpr] -> Set.Set String -> [Spec.RelExpr]
useVars invs vs = filter (\v -> (`Set.member` vs) `any` gatherVars [v]) invs


{-# NOINLINE logme #-}
logme :: [String] -> [String]
logme vs = vs -- unsafePerformIO (putStrLn "holes: 1") `seq` vs

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
dimzero d s = case Map.lookup s d of 
  Just v -> v == 0
  Nothing -> error $ "missing dim variables " ++ s

compSpecs t = map (specToBoogie [] . Spec.inlinePrev . t)

makeAssumptsRels rs = makeAssumpts $ map (specToBoogie []) rs

makeAssumpts :: [Expression] -> Block
makeAssumpts = map worker
  where
    worker e = gen ([], gen $ Predicate [] $ SpecClause Inline True e)


makeAsserts :: [Expression] -> Block
makeAsserts = map worker
  where
    worker e = gen ([], gen $ Predicate [] $ SpecClause Inline False e)


checkInvs :: ([Spec.RelExpr], [Spec.RelExpr]) -> [String] -> (Block, Program, Body) -> Bool
checkInvs (staleInvs, fineInvs) globals x@(blk, Program decs, scope@(vs,_)) = checkProg checkme
  where 
    posts = compSpecs id fineInvs
    (pres, posts') = (compSpecs Spec.weakenPrev staleInvs ++ posts, posts ++ compSpecs id staleInvs) 
    checkme = optimize $ Program $ decs ++ [buildMain pres posts' globals (vs, blk)]

{-# NOINLINE assertFixed #-}
assertFixed :: ([Spec.RelExpr], [Spec.RelExpr]) -> [String] -> (Block, Program, Body) -> (Block, Program, Body)
assertFixed is gs st = if checkInvs is gs st then st else error "Expected correct program"


fixBlock :: DimEnv -> [Spec.RelExpr] -> [Set.Set String] -> Program -> [String] -> [String] -> Set.Set String -> Body -> Block -> Block -> (Block, Program, Body)
fixBlock dims invs relVars header globals rhsVars stales scope prefix fixme = fixResult inner
  where
    genSkel s = [genStart s, genHole s, genEnd s]
    inner = foldl wrapFixStmt initState fixme
    fixResult :: (Block, Block, Program, Body) -> (Block, Program, Body)
    fixResult (_, blk, prog@(Program decs), scope'@(vs, _)) = if not isRepaired then fixed else (blk, prog, scope')
      where

        isRepaired = checkInvs (staleInvs, fineInvs) globals (blk, prog, scope')
        
        staleVars = Set.toList $ stales `Set.union` Set.fromList (findEdited blk)

        (staleInvs, fineInvs) = partition (isRelated staleVars relVars) invs


        candVars = rhsVars \\ staleVars

        -- (invs', builder) = transRels staleInvs

        pres = compSpecs Spec.weakenPrev staleInvs ++ compSpecs id fineInvs

        

        (suffix, prog', bod') = createFix dims staleInvs prog globals candVars (filter (dimzero dims) rhsVars) scope' blk 

        fixed = assertFixed (staleInvs, fineInvs) globals (makeAssumpts pres ++ suffix, prog', bod')

    initState = (prefix, [], header, scope)
    wrapFixStmt :: (Block, Block, Program, Body) -> LStatement -> (Block, Block, Program, Body)
    wrapFixStmt (pref, blk, prog, bod) (Pos o (ls, Pos i s@(While _ _ b))) = (pref', blk'', prog'', bod')
      where
        (idx, loopVars, arrVs) = parseLoopInfo $ head b
        (loopVs, arrs) = (Set.fromList loopVars, Set.fromList arrVs)
        newStales = loopVs `Set.intersection` findEditedDeep b
        (s', prog'', bod') = fixStmt dims invs relVars globals rhsVars (newStales `Set.difference` arrs) (pref, prog', scope') s
        prefix = case s' of 
          x@While{} -> makeAssumptsRels invs -- TODO: this doesn't actually work because the dims are wrong. see check.bpl for details.
          _         -> [] 
        (blk', prog', scope') = fixBlock dims invs relVars prog globals rhsVars stales bod pref []
        blk'' = blk' ++ prefix ++ [Pos o (ls, Pos i s')] ++ prefix

        pref' = blk''

    -- fix everything up before conditionals
    wrapFixStmt (pref, blk, prog, bod) (Pos o (ls, Pos i s@(If e tru fls))) = (pref', blk', prog'', bod'')
      where
        (prefix, prog', bod') = fixBlock dims invs relVars prog globals rhsVars stales bod pref []
        (s', prog'', bod'') = fixStmt dims invs relVars globals rhsVars Set.empty (pref ++ prefix, prog', bod') s
        fix = prefix ++ [Pos o (ls, Pos i s')]
        blk' = blk ++ fix
        pref' = pref ++ fix
    wrapFixStmt (pref, blk, prog, bod) (Pos o (ls, Pos i s)) = (pref', blk', prog', bod')
      where
        (s', prog', bod') = fixStmt dims invs relVars globals rhsVars stales (pref, prog, bod) s
        prefix = case s' of 
          x@While{} -> makeAssumptsRels invs -- TODO: this doesn't actually work because the dims are wrong. see check.bpl for details.
          _         -> [] 
        fix = prefix ++ [Pos o (ls, Pos i s')] ++ prefix
        blk' = blk ++ fix
        pref' = pref ++ fix

    buildLS s = gen ([], gen s)

    createFix :: DimEnv -> [Spec.RelExpr] -> Program -> [String] -> [String] -> [String] -> Body -> Block -> (Block, Program, Body)
    createFix dims invs prog globals lvars rvars scope oldblk = ret
      where
        -- initState = (dims, invs, scope, templ, Set.fromList lvars, Set.fromList rvars)
        -- canSynth (ds, _, _, _, lvs, _) = all (dimzero ds) lvs

        -- worker (odims, oinvs, oscope, blk, lvs, rvs) = (dims', invs', scope', blk', lvs', rvs')
        --   where 
        --     lvs' = okvs `Set.union` Set.fromList loopVars
        --     rvs' = rvs `Set.union` Set.filter (dimzero dims') (Set.fromList (idx : loopVars))
        --     (okvs, arrvs) = Set.partition (dimzero odims) lvs

        --     loopDecs = map genLoopV $ Set.toList arrvs

        --     genLoopV arrV = ("cegis_loop_var", lty)
        --       where
        --         lty = case buildTy $ (Map.!) odims arrV of 
        --           (Imp.ArrTy i) -> i
        --           _         -> error $ "expected array variable in arrVs, instead found " ++ arrV

        --     (itws, r) = oscope

        --     (loopSS, newItws) = translateStmt (Imp.For loopDecs (Just "cegis_loop_idx") (map Imp.VConst $ Set.toList arrvs) (Imp.Seq []), concat itws)
        --     scope' = ([newItws], r)

        --     (oldPre, oldLoop) = (init blk, last blk)

  

        --     blk' = if null blk then loopBLK else oldPre ++ insertIntoLoop loopBLK (Just oldLoop)

        --     (newLoopPre, While c spec bod) = (init loopSS, last loopSS)
        --     loopSS' = newLoopPre ++ [While c spec bod']
        --     loopBLK = map (\s -> gen ([], gen s)) loopSS'

        --     -- holes = filter (dimzero dims') loopVars >>= genSkel
        --     holes = []

        --     (idx, loopVars, arrVs) = parseLoopInfo $ head bod
        --     (pref, mid, suf) = parseLoop bod
    
        --     --(relInvs, unrelInvs) = partition (isRelated relVars rhsVars) invs
        --     invs' = map (specializeSpec loopVars arrVs idx) oinvs 
    
        --     -- spec' = spec ++ map buildLoopInv invs
        --     pres = compSpecs Spec.weakenPrev invs'
        --     bod' = pref ++ holes ++ makeAssumpts pres ++ mid ++ suf -- ++ map buildLoopCheck
    
        --     dims' = addDims odims loopDecs `Map.union` Map.singleton idx 0


        -- (finalDims, finalInvs, synthScope, skel, lvs, rvs) = until canSynth worker initState
        -- templ = concat [[genStart s, genHole s, genEnd s] | s <- filter (dimzero dims) lvars]

 

        -- skel'
        --   | canSynth initState = buildCond snippets
        --   | null skel = insertIntoLoop (buildCond snippets) Nothing
        --   | otherwise = front skel ++ insertIntoLoop (buildCond snippets) (Just $ last skel) ++ makeAssumptsRels invs

        invRels = relatedFromInvs invs
        isStale = varRelated invRels (gatherVars invs)

        determined = Set.fromList $ gatherVars invs \\ lvars
        problems = calculateCegisProblems determined (Set.fromList $ filter isStale lvars) invs

        (rblk, rprog, rbod, _, _) = foldl' buildRet (oldblk, prog, scope, [], dims) problems

        ret = (rblk, rprog, rbod)

        
       
        -- ret = (foldl (flip trimBlock) rblk (Set.toList lvs), rprog, rbod)
        -- ret = error "TODO"

        buildRet :: (Block, Program, Body, [Spec.RelExpr], DimEnv) -> (Set.Set String, [Spec.RelExpr]) -> (Block, Program, Body, [Spec.RelExpr], DimEnv)
        buildRet (blk, prog, bod, curInvs, env) (lvalues, locInvs) = (blk ++ blk', prog', ([itws'], b), newInvs, env')
          where

            newInvs = curInvs ++ locInvs
            (pres, posts) = (compSpecs Spec.weakenPrev (if null curInvs then invs else curInvs), compSpecs id newInvs)


            (env', bindings, specInvs, lvs, skel) = generateSubProblems env (lvalues, locInvs) Map.empty []

            invRels = relatedFromInvs locInvs
            isStale = varRelated invRels (gatherVars newInvs)

            lvs' = Set.filter (\s -> isStale s && dimzero env' s) lvs

            snip = concatMap genSkel lvs'
            assumpts = []

            (fixed, prog', bod') = repairBlock pres posts prog globals (Set.toList lvs') (gatherVars newInvs) bod blk snip assumpts
            fixed' = if null assumpts then fixed else trimCond fixed
            fixes = parseFixes (Set.toList lvalues) fixed'

            (itws, b) = bod'


            newSpyBlk = combineSynSolutions env' skel (rebuildFix $ concat $ Map.elems fixes)
            (blk', itws') = translateBlock (newSpyBlk, concat itws)

            -- snippets = genCegPs (Set.toList lvs') locInvs



        --     (skel', prog', bod') = foldl inner (skel, prog, bod) snippets
        --     snippets = genCegPs (Set.toList lvalues) locInvs
        --     inner (sk, p, b) (invs, snip) = (sk', p', b')
        --       where
        --         (assumpts, snip') = completeCond (Set.toList lvalues) snip
        --         (fixed, p', b') = repairBlock pres posts p globals (Set.toList lvalues) (gatherVars invs) b (oldblk ++ sk) snip' assumpts
        --         fixed' = if null assumpts then fixed else trimCond fixed
        --         fixes = parseFixes (Set.toList lvalues) fixed'
        --         sk' = rebuildBlock sk fixes






calculateCegisProblems :: Set.Set String -> Set.Set String -> [Spec.RelExpr] -> [(Set.Set String, [Spec.RelExpr])]
calculateCegisProblems srcs lvars invs = schedule
  where
    schedule = init $ scheduleInvs srcs lvars invs
  
    -- ** convert the lvars + invariants to a template for synthesis:
    --   *** group lvars by dims. dim k >= 1 goes (together) in a loop of depth k.
    --   *** flatten invariants, lvars until all lvars are depth 0. collect each set of flattened lvars.
generateSubProblems :: DimEnv -> (Set.Set String, [Spec.RelExpr]) -> Map.Map String String -> [([String], String, [String])] -> (DimEnv, Map.Map String String, [Spec.RelExpr], Set.Set String, [([String], String, [String])])
generateSubProblems env (lvars, invs) bindings skel = 
  if Set.null complexVars then (env, bindings, invs, lvars, skel) else generateSubProblems env' (lvars', invs') bindings' skel'
  where
    (doneVars, complexVars) = Set.partition (dimzero env) lvars
   
    (loopVars, withLoop) = foldl adder (Map.empty, env) (Set.toList complexVars)
    (idxVar, env') = allocFreshSpy "cegis_idx_var" (buildTy 0) withLoop
    invs' =  map (specializeSpec (Map.elems loopVars) (Set.toList complexVars) idxVar) invs 
    bindings' = Map.union bindings loopVars
    skel' = (Map.elems loopVars, idxVar, Map.keys loopVars) : skel

    lvars' = doneVars `Set.union` Set.fromList (Map.elems loopVars)

    adder :: (Map.Map String String, DimEnv) -> String -> (Map.Map String String, DimEnv)
    adder (vars, e) var = 
      let ty = buildTy $ (Map.!) e var - 1
          (newVar, newE) = allocFreshSpy "cegis_loop_var" ty e
      in
        (Map.insert var newVar vars, newE)



-- *** foreach set of flattened lvars, put the conditional case into a loop over the flattened var. repeat until the original loop is ready to go.
combineSynSolutions :: DimEnv -> [([String], String, [String])] -> Imp.Block -> Imp.Block
combineSynSolutions env [] blk = blk
combineSynSolutions env ((iters, idx, arrs):skels) inner = 
  combineSynSolutions env skels $ Imp.Seq [Imp.For iterTups (Just idx) (map Imp.VConst arrs) inner]
  where
    iterTups :: [Imp.VDecl]
    iterTups = map (\s -> (s, buildTy $ (Map.!) env s)) iters


buildCond :: [([Spec.RelExpr], Block)] -> Block
buildCond snips = join (let (_, x) = unzip snips in x)

convertConds :: [([Spec.RelExpr], Block)] -> [([Spec.RelExpr], Block)]
convertConds snips = snips >>= worker
  where
    worker (specs, b@[Pos _ ([], Pos _ (If (Expr co) t f))]) = if null conds then [(specs,b)] else branches
      where 
        conds     = specs >>= Spec.gatherConds
        branches  = map (\cn -> (specs, singletonBlock (gen $ If (Expr $ gen $ BinaryExpression And co $ specToBoogie [] cn) t f))) conds
    worker (specs, b) = [(specs,b)]

      

genCegPs :: [String] -> [Spec.RelExpr] -> [([Spec.RelExpr],Block)]
genCegPs vs specs = convertConds $ if any isImp canon then foldl worker [] canon else [(specs, assigns')]
  where
    assigns = concat [[genStart s, genHole s, genEnd s] | s <- vs]
    assigns' = singletonBlock (gen $ If (Expr $ specToBoogie [] (Spec.RelBool True)) assigns Nothing)
    canon = until allBase splitAnds specs
    allBase = all (not . isAnd)

    splitAnds ss = ss >>= f
      where 
        f (Spec.RelBinop Spec.And l r) = [l, r]
        f s = [s]

    worker acc x@(Spec.RelBinop Spec.Imp l _) = acc ++ [(x:filter (unrelated l) canon, singletonBlock (gen $ If (Expr $ specToBoogie [] l) assigns Nothing))]
    worker acc _ = acc

    unrelated :: Spec.RelExpr -> Spec.RelExpr -> Bool
    unrelated l (Spec.RelBinop Spec.Imp l' _) = null $ gatherVars [l] `intersect` gatherVars [l']
    unrelated _ _ = True 

    isImp (Spec.RelBinop Spec.Imp _ _) = True
    isImp _ = False
    isAnd (Spec.RelBinop Spec.And _ _) = True
    isAnd _ = False

completeCond :: [String] -> Block -> (Block, Block)
completeCond vs b = case uncons $ b >>= bs2lss worker of 
                              Just (assumpt@(Pos _ ([], Pos _ (Predicate [] (SpecClause Inline True _)))), x@(Pos _ ([], Pos _ If{})):xs) -> ([assumpt], x:xs)
                              Just (x, y) -> ([], x:y)
                              Nothing -> ([], [])

  where
    worker (If c t _) = [pref, If c t $ Just f]
      where 
        f = [] --(stmt $ Havoc vs) : [stmt $ Predicate [] (SpecClause Inline True $ specToBoogie [] i) | i <- invs]
        pref = case c of (Expr c') -> Predicate [] (SpecClause Inline True c')
    worker s = [s]

trimCond :: Block -> Block
trimCond b = b >>= bs2lss worker
  where
    worker (If c t _) = map strip t
    worker s = [s]

    strip x = let (_, r) = node x in node r

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
      Nothing -> mp
   
specializeSpec _ _ _ x = x

-- allocate a bunch of previous variables if necessary
-- assumes it is only called for integer iterators
generatePrevs :: [String] -> Body -> ([String], Body)
generatePrevs vs scope = foldl worker ([], scope) vs
  where
    worker :: ([String], Body) -> String -> ([String], Body)
    worker (accV, accScope) nxt = 
      let (newV, newScope) = allocIfMissing nxt IntType accScope in 
        (newV:accV, newScope)

buildSelfAssgn :: String -> LStatement
buildSelfAssgn v = gen ([], gen $ Assign [(v, [])] [gen $ Var v])


fixStmt :: DimEnv -> [Spec.RelExpr] -> [Set.Set String] -> [String] -> [String] -> Set.Set String -> (Block, Program, Body) -> BareStatement -> (BareStatement, Program, Body)
fixStmt dims invs relVars globals rhsVars stales (prefix, prog, scope) = worker
   where
    worker (If e tru fls) = (If e tru' (Just fls'), prog'', scope'')
      where
        (tru', prog', scope') = recur prog scope tru
        -- (fls', prog'', scope'') = ([], prog', scope')
        (fls', prog'', scope'') = recur prog' scope' (fromMaybe [] fls) 
    worker (While c spec bod) = (While c spec bod', prog', scope')
      where
        (idx, vs, arrs) = parseLoopInfo $ head bod
        (pref, mid, suf) = parseLoop bod
        relVars' = relVars ++ [Set.fromList vs]
        stales' = stales `Set.union` (Set.fromList vs `Set.intersection` findEditedDeep bod) `Set.difference` Set.fromList arrs
        (fixed, prog', scope') = fixBlock dims invs'' relVars' prog globals' rhsVars' stales' prevScope prefix (prevUpdates ++ mid)

        (relInvs, unrelInvs) = partition (isRelated arrs relVars') invs
        invs' = map (specializeSpec vs arrs idx) relInvs ++ unrelInvs
        -- in addition, introduce new variables for the prevs as necessary and updatePrevs on the invs.
        baseVs = filter (dimzero dims) vs
        (pVars, prevScope) = generatePrevs (map ("prev_" ++) baseVs) scope
        invs'' = map (updatePrevs (Map.fromList $ baseVs `zip` repeat idx)) invs'

        (pres, posts) = (compSpecs Spec.weakenPrev invs'', compSpecs id invs'')

        prevUpdates = [] --map buildSelfAssgn pVars

        -- prefix = prevUpdates ++ prefix 

        globals' = globals --[]
        rhsVars' = vs

        -- spec' = spec ++ map buildLoopInv invs
        bod' = pref ++ makeAssumpts pres ++ fixed ++ makeAsserts posts ++ suf

      --   spec' = (map (SpecClause LoopInvariant False) invs) ++ spec
      --   (tru', prog', bod') = recur prog scope tru
      --   (fls', prog'', bod'') = case fls of 
      --     Just i  -> let (r, p, b) = recur prog' bod' i in (Just r, p, b)
      --     Nothing -> (Nothing, prog', bod')
    worker s = (s, prog, scope)
    recur p b = fixBlock dims invs relVars p globals rhsVars stales b prefix

isRelated :: [String] -> [Set.Set String] -> Spec.RelExpr -> Bool
isRelated vars rels e = not $ null intersection
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

findEditedDeep :: Block -> Set.Set String
findEditedDeep blk = foldl Set.union Set.empty $ map (recur . node . snd . node) $ stripLoopInfo blk
  where
    recur :: BareStatement -> Set.Set String
    recur (Assign assns _) = Set.fromList $ map fst assns
    -- recur x@Assign{} = Set.empty
    recur (While _ _ b) = findEditedDeep b
    recur (If _ t f) = findEditedDeep t `Set.union` findEditedDeep (fromMaybe [] f)
    recur x@Predicate{} = Set.empty
    -- recur (If _ t f) = findEditedDeep t `Set.union` findEditedDeep (fromMaybe [] f)
    




findUnedited :: [Set.Set String] -> Block -> [String]
findUnedited rels blk = computeRels used rels \\ used
  where
    used = findEdited blk

findInScope :: Program -> Body -> [Spec.RelExpr] -> [String]
findInScope (Program decs) _ desugInvs = (decs >>= (getVars . node)) `intersect` invVars
  where
    getVars (VarDecl itws) = map itwId itws
    getVars _ = []
    invVars = gatherVars desugInvs



stripLoopInfo :: Block -> Block
stripLoopInfo blk = fst $ foldl recur ([], False) $ map (node . snd . node) blk
  where
    recur :: (Block, Bool) -> BareStatement -> (Block, Bool)
    recur (acc, True) (Predicate [Attribute "forBegin" _] _) = (acc, False)
    recur (acc, True) _ = (acc, True)
    recur (acc, False) (Predicate [Attribute "forInfo" _] _) = (acc, True)
    recur (acc, False) (Predicate [Attribute "forEnd" _] _) = (acc, True)
    recur (acc, False) (While c s b) = (acc ++ wrap (While c s (stripLoopInfo b)), False)
    recur (acc, False) (If c t f) = (acc ++ wrap (If c (stripLoopInfo t) (stripLoopInfo `fmap` f )), False)
    recur (acc, False) s = (acc ++ wrap s, False)

    wrap :: BareStatement -> [LStatement]
    wrap bs = [gen ([], gen bs)]

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