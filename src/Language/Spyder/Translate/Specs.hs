{-# LANGUAGE LambdaCase #-}

module Language.Spyder.Translate.Specs (
    prefixApps
  , saturateApps
  , specToBoogie
  , dimVars
  , inlineApps
) where

import qualified Language.Boogie.AST as BST
import qualified Language.Boogie.Position as Pos
import Language.Spyder.AST.Spec
import Language.Spyder.Translate.Rename             (alphaRel)
import qualified Data.Map.Strict as Map

saturateApps :: RelExpr -> RelExpr
saturateApps = id --recur
  where
    recur :: RelExpr -> RelExpr
    recur (RelApp f (v@(RelVar x):vs)) = RelApp f $ v:vs ++ [RelVar $ x ++ "$dim0"]
    recur _ = error "TODO"



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
    recur _ = error "TODO"
      

prefixApps :: String -> RelExpr -> RelExpr
prefixApps pref = recur
  where 
    recur :: RelExpr -> RelExpr
    recur (RelApp f args) = RelApp (pref ++ f) $ map recur args
    recur (RelBinop o l r) = RelBinop o (recur l) (recur r)
    recur (RelUnop o i) = RelUnop o (recur i)
    recur (Foreach vs idx arrs bod) = Foreach vs idx arrs $ recur bod
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




specToBoogie :: [String] -> RelExpr -> BST.Expression
specToBoogie = recur
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
          Nothing -> Map.empty
        inner' = Pos.gen $ BST.BinaryExpression BST.Implies validIdx (alphaIdx (vs `zip` arrs) qv inner)
        validIdx = Pos.gen $ BST.BinaryExpression BST.And lo hi
        lo = Pos.gen $ BST.BinaryExpression BST.Geq (Pos.gen $ BST.Var qv) (Pos.gen $ BST.numeral 0)
        hi = Pos.gen $ BST.BinaryExpression BST.Gt (Pos.gen $ BST.Var d) (Pos.gen $ BST.Var qv)
    recur _ (RelVar x) = Pos.gen $ BST.Var x
    recur _ (RelInt x) = Pos.gen $ BST.Literal $ BST.IntValue $ fromIntegral x
    recur _ (RelBool x) = Pos.gen $ BST.Literal $ BST.BoolValue x
    recur _ _ = error "TODO"

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