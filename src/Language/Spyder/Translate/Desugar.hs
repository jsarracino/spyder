module Language.Spyder.Translate.Desugar (
    uniqifyNames
  , stripLt
  , gatherDecls
  , convertArr
  , translateArrs
  , generateBoogieBlock
) where

import Language.Spyder.AST.Imp
import qualified Data.Map.Strict as Map
import Data.Set (union, empty, Set, singleton, toList)
import Control.Monad (liftM)

-- need to know the dimensions of each array.
type ArrInfo = VDecl



-- desugar lt into ! (geq)
stripLt :: Block -> Block
stripLt (Seq ss) = Seq $ map stmtWorker ss
  where
    stmtWorker (Decl d rhs) = Decl d $ liftM exprWorker rhs
    stmtWorker (Assgn l r) = Assgn (exprWorker l) (exprWorker r)
    stmtWorker (For vs arrs bod) = For vs (map exprWorker arrs) (stripLt bod)
    stmtWorker (While c b) = While (exprWorker c) (stripLt b)
    stmtWorker (Cond c tr fl) = Cond (exprWorker c) (stripLt tr) (stripLt fl)

    exprWorker x@(VConst _) = x
    exprWorker x@(IConst _) = x
    exprWorker x@(BConst _) = x
    exprWorker (AConst vs) = AConst $ map exprWorker vs
    exprWorker (UnOp o i) = UnOp o $ exprWorker i
    exprWorker (BinOp Lt l r) = UnOp Not $ BinOp Ge l r
    exprWorker (BinOp o l r) = BinOp o (exprWorker l) (exprWorker r)
    exprWorker (Index l r) = Index (exprWorker l) (exprWorker r)
    exprWorker (App l r) = App (exprWorker l) (map exprWorker r)

-- gather decls into a thinger and convert to assignments
gatherDecls :: Block -> Set VDecl
gatherDecls (Seq ss) = foldl union empty (map stmtWorker ss)
  where
    stmtWorker (Decl d _) = singleton d
    stmtWorker (For _ _ bod) = gatherDecls bod -- assumes vs have been moved around
    stmtWorker (While _ bod) = gatherDecls bod
    stmtWorker (Assgn _ _) = empty
    stmtWorker (Cond _ tr fl) = gatherDecls tr `union` gatherDecls fl
-- transform all decls to assigns
-- we use a concatMap because a blank decl e.g. let foo: int should be hoisted and just elided
convertDecls :: Block -> Block
convertDecls (Seq ss) = Seq $ concatMap stmtWorker ss
  where
    stmtWorker (Decl (l,_) (Just r)) = [Assgn (VConst l) r]
    stmtWorker (Decl _ Nothing) = []
    stmtWorker (For vs ars bod) = [For vs ars (convertDecls bod)]
    stmtWorker (While c bod) = [While c (convertDecls bod)]
    stmtWorker (Cond c tr fl) = [Cond c (convertDecls tr) (convertDecls fl)]
    stmtWorker x@(Assgn _ _) = [x]


-- convert an array constant into an initialization sequence, as well
-- as an ArrInfo. the sequence is prefixed by variable decls for the array's variables.
convertArr :: Integer -> Type -> [Expr] -> ([Statement], String, ArrInfo)
convertArr suffix bty es = (decls ++ assns, arrVar, arrInfo)
  where
    lenVar = "_len" ++ show suffix
    len = IConst $ length es
    arrVar = "_arr" ++ show suffix
    arrDecl = (arrVar, ArrTy bty)
    arr = VConst arrVar
    decls = [Decl arrInfo (Just len), Decl arrDecl Nothing]
    arrInfo = (lenVar, BaseTy "int")
    assns = zipWith worker es [0..]
    worker e i = Assgn (Index arr (IConst i)) e

type ArrInfos = Map.Map String ArrInfo

type TranslateState = (Integer, ArrInfos)

translateArrs :: Block -> TranslateState -> (Block, TranslateState)
translateArrs (Seq ss) (initSeed, oldArrs) = let (newSeed, arrs, ss') = foldl sWorker start ss in
  (Seq ss', (newSeed, arrs))
  where
    start = (initSeed, oldArrs, [])
    sWorker (seed, arrs, acc) s = case s of
      (Decl (v, ty) (Just (AConst es))) ->
        let (ss', arrNme, arrInfo) = convertArr seed (unpack ty) es in
        let s' = Decl (v, ty) (Just $ VConst arrNme) in
        (seed+1, Map.insert v arrInfo arrs, acc ++ ss' ++ [s'])
      -- (Assgn l r@(AConst _)) ->
      --   let (ss', arrNme, arrInfo) = convertArr seed (unpack ty) r in
      --   let s' = Assgn l (Just $ VConst arrNme) in
      --   (seed+1, Map.insert l arrInfo arrs, acc ++ [s'] ++ ss')
      (Assgn _ _) -> (seed, arrs, acc ++ [s]) -- TODO: make work for rhs constants...maybe
      (Decl _ _) -> (seed, arrs, acc ++ [s])
      -- assumes arr primitives in arrs have been hoisted above the loop
      (For vs vars bod) ->
        let (bod', (seed', arrs')) = translateArrs bod (seed, arrs) in
        (seed', arrs', acc ++ [For vs vars bod'])
      (While c bod) ->
        let (bod', (seed', arrs')) = translateArrs bod (seed, arrs) in
        (seed', arrs', acc ++ [While c bod'])
      (Cond c tr fl) -> 
        let (tr', (seed', arrs'))    = translateArrs tr (seed, arrs) 
            (fl', (seed'', arrs'')) = translateArrs fl (seed', arrs') in
          (seed'', arrs'', acc ++ [Cond c tr' fl'])
        -- (seed', arrs', acc ++ [While c bod'])

    unpack (ArrTy ty) = ty
    unpack x@_ = undefined $ "Type error, not an array type: " ++ show x

-- convert for-in loop to while loop
-- add a decl for each parallel assignment, and loop while the indices
-- are within the length. add assignments for bumping the indices at the end of the loop.

-- convert for (decls) in (arrs) bod to
-- [let decl <- arr[arr_index]]
-- while (BIGAND [arr_index < arr_length]) Seq [decl <- arr[arr_index]] ++ bod ++ [arr[arr_index] <- decl] ++ [arr_index <- arr_index + 1]
convertForStmt :: ArrInfos -> Statement -> ([Statement], Statement)
convertForStmt arrInfo (For vs arrs (Seq bod)) = (decls, loop)
  where
    loop = While cond (Seq bod')
    idx (VConst v) = v ++ "_idx"
    idx _ = undefined "Error: can only loop over variables (for now)"
    varIdx = VConst . idx
    len a =
      Index a (IConst (-1))
    len _ = undefined "Error: can only loop over variables (for now)"
    bnds = vs `zip` arrs
    decls = map buildIndex arrs ++ map buildIdent bnds
    buildIndex arr = Decl (idx arr, BaseTy "int") (Just $ IConst 0)
    buildIdent (v, arr) = Decl v (Just $ Index arr (varIdx arr))
    cond = foldl (BinOp And) tru arrConds
    arrConds = map (\arr -> BinOp Lt (varIdx arr) (len arr)) arrs
    tru = BConst True
    incr i = Assgn i (BinOp Plus i (IConst 1))

    preUpdates = undefined "TODO"
    postUpdates = undefined "TODO"
    -- TODO: need to calculate the actual updates to iteration variables
    -- i.e. itr <- arr[idx]
    -- at both beginning and end of loop, but before the idxs are incremented

    bod' = preUpdates ++ bod ++ postUpdates ++ map (incr . varIdx) arrs
convertForStmt _ x@_ = ([], x)

-- desugarLoopS :: ArrInfo -> Statement ->
convertForBlock :: ArrInfos -> Block -> Block
convertForBlock arrs (Seq ss) = Seq $ concatMap worker ss'
  where
    ss' = map (convertForStmt arrs) ss
    worker (pres, s) = pres ++ [s]

-- variable renaming, add an int to the end of each variable name for uniqification ;)
uniqifyNames :: Block -> Block
uniqifyNames x = (Seq . snd) $ blockWorker start x
  where
    start = Map.empty
    blockWorker vars (Seq ss) = foldl stmtWorker (vars, []) ss
    stmtWorker (vars, ss) s = case s of
      (Decl (nme, ty) rhs) ->
        let suffix = get nme vars + 1 in
        let nme' = nme ++ show suffix in
        let rhs' = liftM (renameExpr vars) rhs in
        let vars' = Map.insert nme suffix vars in
        (vars', ss ++ [Decl (nme', ty) rhs'])
      (Assgn l r) ->
        let s' = Assgn (renameExpr vars l) (renameExpr vars r) in
        (vars, ss ++ [s'])
      -- assumes vs have been lowered into/hoisted above bod
      (For vs arrs bod) ->
        let (vars', bod') = blockWorker vars bod in
        (vars', ss ++ [For vs arrs (Seq bod')])
      (While c bod) ->
        let (vars', bod') = blockWorker vars bod in
        (vars', ss ++ [While c (Seq bod')])

    get = Map.findWithDefault 0
    renameExpr vars (VConst v) = VConst $ merge v vars
    renameExpr vars (BinOp o l r) =
      let recur = renameExpr vars in
      let (l', r') = (recur l, recur r) in
      BinOp o l' r'
    renameExpr vars (UnOp o i) = UnOp o $ renameExpr vars i
    renameExpr vars (Index l r) =
      let recur = renameExpr vars in
      let (l', r') = (recur l, recur r) in
      Index l' r'
    renameExpr _ z@(IConst _) = z
    renameExpr _ z@(BConst _) = z
    renameExpr _ (AConst _) = undefined "Error: rename assumes arrays are lifted"
    merge nme vars =
      let suffix = get nme vars in
      nme ++ show suffix

-- phases: the ordering within a phase doesn't matter but the order
-- between phases does
-- 0) ** array constants -- introduces decls
-- 1) ** loop conversion -- introduces conditions, decls
-- 2) everything else:
--    ** uniqify names -- introduces decls
--    ** condition simplification
-- 3) decl hoisting      -- not a simplification but needs to be run before decl conversion
-- 4) decl conversion    -- removes decls

-- the resulting block can be directly translated to boogie code
generateBoogieBlock :: Block -> ([VDecl], [Statement])
generateBoogieBlock b =
  let start = (0, Map.empty) in
  let (convArrs, (_, arrInfos)) = translateArrs b start in
  let passes = [stripLt] in
  -- let passes = [stripLt, uniqifyNames, convertForBlock arrInfos] in
  let beforeDecs = foldr (.) id passes convArrs in
  let decls = gatherDecls beforeDecs in
  let (Seq final) =  convertDecls beforeDecs in
  (toList decls, final)

  