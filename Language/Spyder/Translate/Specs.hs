{-# LANGUAGE LambdaCase #-}

module Language.Spyder.Translate.Specs (
    prefixApps
  , specToBoogie
  , dimVars
  , inlineApps
  , fillForeach
  , renamePrev
  , updatePrevs
) where

import qualified Language.Boogie.AST as BST
import qualified Language.Boogie.Position as Pos
import Language.Spyder.AST.Spec
import Language.Spyder.Translate.Rename             (alphaRel)
import qualified Data.Map.Strict as Map

import Data.Maybe




inlineApps :: Map.Map String ([String], RelExpr) -> RelExpr -> RelExpr
inlineApps decs = recur
  where
    recur e@(RelApp f args) = 
      let (formals, bod) = (Map.!) decs f
          binds          = Map.fromList $ formals `zip` args
      in recur $ alphaRel binds bod
    recur (RelBinop o l r) = RelBinop o (recur l) (recur r)
    recur (RelUnop o i) = RelUnop o (recur i)
    recur (Foreach vs idx arrs bod) = Foreach vs idx arrs $ recur bod
    recur x@RelVar{} = x
    recur x@RelInt{} = x
    recur x@RelBool{} = x
    recur (RelCond c t f) = RelCond (recur c) (recur t) (recur f)
    recur (Prev v i) = Prev v (recur i)
    -- recur x@RelIndex{} = x
      

prefixApps :: String -> RelExpr -> RelExpr
prefixApps pref = recur
  where 
    recur :: RelExpr -> RelExpr
    recur (RelApp f args) = RelApp (pref ++ f) $ map recur args
    recur (Prev f i) = Prev f $ recur i
    recur (RelBinop o l r) = RelBinop o (recur l) (recur r)
    recur (RelUnop o i) = RelUnop o (recur i)
    recur (Foreach vs idx arrs bod) = Foreach vs idx arrs $ recur bod
    recur (RelCond c t f) = RelCond (recur c) (recur t) (recur f)
    recur x@RelVar{} = x
    recur x@RelInt{} = x
    recur x@RelBool{} = x
    recur _ = error "TODO"

specBop :: RelBop -> BST.BinOp
specBop = \case
  Plus -> BST.Plus
  Minus -> BST.Minus
  Mul -> BST.Times
  Div -> BST.Div
  Lt -> error "Error: translation assumes LT has been desugared"
  Le -> BST.Leq
  Gt -> BST.Gt
  Ge -> BST.Geq
  And -> BST.And
  Or -> BST.Or
  Eq -> BST.Eq
  Neq -> BST.Neq
  Imp -> BST.Implies
  Iff -> BST.Equiv
  Mod -> BST.Mod

specUnop :: RelUop -> BST.UnOp
specUnop = \case
  Neg -> BST.Neg
  Not -> BST.Not


-- generate a list of variables for array dimensions. very hackish, TODO
dimVars :: RelExpr -> [String]
dimVars e = map (\i -> "dim" ++ show i) [0..quantDepth e -1]


quantDepth :: RelExpr -> Int
quantDepth (RelUnop _ i) = quantDepth i
quantDepth (RelBinop _ l _ ) = quantDepth l -- TODO: only works if both are the same dimension. also, we don't currently have binops over arrays.
quantDepth (Foreach _ _ _ bod) = 1 + quantDepth bod
quantDepth _ = 0 -- App, Var, Int, Bool


-- similar to renamePrev, but in this case, we're updating a (possibly bare) prev to the variable version
-- e.g. updatePrev prev(foo,0) => prev_var(prev_foo,0,idx)
-- vs  renamePrev prev(foo,0) => prev(foos,0,idx)
updatePrevs :: Map.Map String String -> RelExpr -> RelExpr
updatePrevs iters = recur
  where
    inIters v = Map.member v iters
    recur (RelApp f args) = RelApp f $ map recur args
    recur (RelBinop o l r) = RelBinop o (recur l) (recur r)
    recur (RelUnop o i) = RelUnop o $ recur i
    recur (Foreach vs i ars b) = Foreach vs i ars $ if any inIters ars then b else recur b
    recur (Prev v i) = case Map.lookup v iters of 
      Just idx -> RelApp "prev_var" [RelVar ("prev_" ++ v), i, RelVar idx]
      Nothing -> Prev v i
    recur x@RelVar{} = x
    recur x@RelInt{} = x
    recur x@RelBool{} = x


-- if x is bound in a foreach like foreach (x: xs), map
-- prev(x, base) => prev(xs, base, idx(x))
-- to do this, save the parent and index of each bound variable in a map
-- assumes that fillForeach has introduces variables for Foreach bindings.
renamePrev :: Map.Map String (String,String) -> RelExpr -> RelExpr
renamePrev = recur
  where
    recur iters (RelApp f args) = RelApp f $ map (recur iters) args
    recur iters (RelCond c t f) = RelCond (recur iters c) (recur iters t) (recur iters f)
    recur iters (RelBinop o l r) = RelBinop o (recur iters l) (recur iters r)
    recur iters (RelUnop o i) = RelUnop o $ recur iters i
    recur iters (Foreach vs (Just v) ars i) = Foreach vs (Just v) ars $ recur (iters `Map.union` Map.fromList (vs `zip` (ars `zip` repeat v))) i
    recur iters (Prev v i) = RelApp "prev" [RelVar parent, RelVar idx, base]
      where 
        (parent, idx) = fromMaybe (error ("missing variable in prev " ++ v)) $ Map.lookup v iters
        base = recur iters i
    recur _ x@RelVar{} = x
    recur _ x@RelInt{} = x
    recur _ x@RelBool{} = x




fillForeach :: RelExpr -> RelExpr
fillForeach (Foreach xs Nothing vs b) = fillForeach (Foreach xs (Just v) vs $  fillForeach b)
  where v = "foreach$" ++ show (quantDepth b)
fillForeach (Foreach xs v@Just{} vs b) = Foreach xs v vs $ fillForeach b
fillForeach (RelApp s args) = RelApp s $ map fillForeach args
fillForeach (RelBinop o l r) = RelBinop o (fillForeach l) (fillForeach r)
fillForeach (RelUnop o i) = RelUnop o (fillForeach i)
fillForeach (RelCond c t f) = RelCond (fillForeach c) (fillForeach t) (fillForeach f)
fillForeach x@RelVar{} = x
fillForeach x@RelInt{} = x
fillForeach x@RelBool{} = x
fillForeach x@Prev{} = x


specToBoogie :: [String] -> RelExpr -> BST.Expression
specToBoogie dims e = recur dims $ renamePrev Map.empty $ fillForeach e
  where
    recur :: [String] -> RelExpr -> BST.Expression
    recur dims (RelApp f args) = Pos.gen $ BST.Application f $ map (recur dims) args
    recur dims (RelBinop Lt l r) = recur dims (RelUnop Not $ RelBinop Ge l r)
    recur dims (RelBinop o l r) = Pos.gen $ BST.BinaryExpression (specBop o) (recur dims l) (recur dims r)
    recur dims (RelUnop o i) = Pos.gen $ BST.UnaryExpression (specUnop o) (recur dims i)
    recur [] x@(Foreach _ _ (a:_) _) = recur names x
      where
        names = map (\suf -> a ++ "$" ++ suf) $ dimVars x
    recur (d:dims) (Foreach vs idx arrs bod) = Pos.gen $ BST.Quantified BST.Forall [] [(qv,BST.IntType)] inner'
      where 
        qv = "foreach$" ++ show (quantDepth bod)
        inner =  recur dims $ alphaRel idxBind bod 
        idxBind :: Map.Map String RelExpr
        idxBind = case idx of 
          Just v  -> Map.singleton v (RelVar qv)
          Nothing -> error "Expected filled foreach"
        inner' = Pos.gen $ BST.BinaryExpression BST.Implies validIdx (alphaIdx (vs `zip` arrs) qv inner)
        validIdx = Pos.gen $ BST.BinaryExpression BST.And lo hi
        lo = Pos.gen $ BST.BinaryExpression BST.Geq (Pos.gen $ BST.Var qv) (Pos.gen $ BST.numeral 0)
        hi = Pos.gen $ BST.BinaryExpression BST.Gt (Pos.gen $ BST.Var d) (Pos.gen $ BST.Var qv)
    
    recur (d:dims) (Prev v i) = recur (d:dims) $ RelApp "prev" [RelVar v,i,RelVar d]
    recur dims (RelCond c t f) = Pos.gen $ BST.IfExpr (recur dims c) (recur dims t) (recur dims f)
    recur _ (RelVar x) = Pos.gen $ BST.Var x
    recur _ (RelInt x) = Pos.gen $ BST.Literal $ BST.IntValue $ fromIntegral x
    recur _ (RelBool x) = Pos.gen $ BST.Literal $ BST.BoolValue x
    
    -- recur ds (RelIndex l r) = Pos.gen $ BST.MapSelection (recur ds l) [recur ds r]

    -- data BareExpression = 
    --   Literal Value |
    --   Var Id |                                        -- ^ 'Var' @name@
    --   Logical Type Ref |                              -- ^ Logical variable
    --   Application Id [Expression] |                   -- ^ 'Application' @f args@
    --   MapSelection Expression [Expression] |          -- ^ 'MapSelection' @map indexes@
    --   MapUpdate Expression [Expression] Expression |  -- ^ 'MapUpdate' @map indexes rhs@
    --   Old Expression |
    --   IfExpr Expression Expression Expression |       -- ^ 'IfExpr' @cond eThen eElse@
    --   Coercion Expression Type |
    --   UnaryExpression UnOp Expression |
    --   BinaryExpression BinOp Expression Expression |
    --   Quantified QOp [Id] [IdType] Expression         -- ^ 'Quantified' @qop type_vars bound_vars expr@

-- alpha-rename each instance of x in expr using the binding (x,arr) to arr[i]
alphaIdx :: [(String, String)] -> String -> BST.Expression -> BST.Expression
alphaIdx renames idx e = Pos.gen $ recur $ Pos.node e
  where 
    mp = Map.fromList renames
    recur :: BST.BareExpression -> BST.BareExpression
    recur v@BST.Literal{} = v
    recur x@(BST.Var v) = case Map.lookup v mp of 
      Just r  -> BST.MapSelection (Pos.gen $ BST.Var r) [Pos.gen $ BST.Var idx]
      Nothing -> x
    recur (BST.Application n args) = BST.Application n (map (Pos.gen . recur . Pos.node) args) 
    recur (BST.UnaryExpression o i) = BST.UnaryExpression o $ Pos.gen $ recur $ Pos.node i
    recur (BST.BinaryExpression o l r) = BST.BinaryExpression o (Pos.gen $ recur $ Pos.node l) (Pos.gen $ recur $ Pos.node r)
    recur (BST.Quantified o tys bs i) = BST.Quantified o tys bs $ Pos.gen $ recur $ Pos.node i
    recur (BST.MapSelection m args) = BST.MapSelection (Pos.gen $ recur $ Pos.node m) $ map (Pos.gen . recur . Pos.node) args
    recur x = x -- Logical,  MapUpdate, old, if, coercion