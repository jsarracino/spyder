module Language.Spyder.Opt (
    constFolding
  , optimize
  , deadCodeElim
  , gatherConstDecs
  , unusedVarElim
)

where

import Language.Boogie.AST
import qualified Data.Map.Strict as Map
import qualified Language.Boogie.Position as Pos
import Language.Spyder.Translate.Direct
import Data.Maybe       (fromMaybe)
import Data.List

import qualified Data.Set as Set

optimize :: Program -> Program
optimize p@(Program decs) = unusedVarElim  $ doAllOpts p 
  where 
    knownVals = gatherConstDecs $ map Pos.node decs
    doAllOpts = foldl (.) id $ map (\x -> x knownVals) [deadCodeElim, constFolding]

gatherConstDecs :: [BareDecl] -> Map.Map String BareExpression
gatherConstDecs decs = Map.fromList $ decs >>= gatherAsn
  where
    gatherAsn (AxiomDecl e) = 
      case Pos.node e of 
        (BinaryExpression Eq l r) -> 
          case (Pos.node l, Pos.node r) of 
            (Var c, Literal v) -> [(c, Pos.node $ unvalue v)]
            (Literal v, Var c) -> [(c, Pos.node $ unvalue v)]
            _                  -> []
          
        _                        -> []
    gatherAsn _ = []

constFolding :: Map.Map String BareExpression -> Program -> Program
constFolding vals (Program decs) = Program $ map simpl decs 
  where
    simpl :: Decl -> Decl
    simpl (Pos.Pos x (ProcedureDecl nme tys args rets con (Just bod))) = Pos.Pos x (ProcedureDecl nme tys args rets con (Just bod'))
      where
        bod' = (fst bod, simplBlk $ snd bod)
    simpl x = x
    simplBlk :: Block -> Block
    simplBlk = map simplLSS
    simplLSS (Pos.Pos x (labels, s)) = Pos.Pos x (labels, simplSS s)
    simplSS (Pos.Pos x (Assign lhs rhs)) = Pos.Pos x $ Assign lhs (map simplE rhs)
    simplSS (Pos.Pos x (If (Expr cond) tr fls)) = Pos.Pos x (If (Expr $ simplE cond) (simplBlk tr) ( simplBlk `fmap` fls))
    simplSS (Pos.Pos x (While (Expr cond) spec bod)) = Pos.Pos x $ While (Expr $ simplE cond) spec (simplBlk bod)
    simplSS (Pos.Pos x (Call lhs f args)) = Pos.Pos x $ Call lhs f (map simplE args)
    simplSS x = x
    simplE :: Expression -> Expression
    simplE (Pos.Pos x (Var v)) = Pos.Pos x $ Map.findWithDefault (Var v) v vals
    simplE (Pos.Pos x (Application l r)) = Pos.Pos x $ Application l (map simplE r)
    simplE (Pos.Pos x (MapSelection l rs)) = Pos.Pos x $ MapSelection l (map simplE rs)
    simplE (Pos.Pos x (BinaryExpression o l r)) = Pos.Pos x $ BinaryExpression o (simplE l) (simplE r)
    simplE (Pos.Pos x (UnaryExpression o i)) = Pos.Pos x $ UnaryExpression o (simplE i)
    simplE x = x
deadCodeElim :: Map.Map String BareExpression -> Program -> Program
deadCodeElim _ (Program decs) = Program $ map simpl decs 
  where
    simpl :: Decl -> Decl
    simpl (Pos.Pos x (ProcedureDecl nme tys args rets con (Just bod))) = Pos.Pos x (ProcedureDecl nme tys args rets con (Just bod'))
      where
        bod' = (fst bod, simplBlk $ snd bod)
    simpl x = x
    simplBlk :: Block -> Block
    simplBlk b = b >>= simplLSS
    simplLSS :: LStatement -> [LStatement]
    simplLSS (Pos.Pos x (labels, s)) = [Pos.Pos x (labels, s') | s' <- simplSS s]
    simplSS :: Statement -> [Statement]
    simplSS (Pos.Pos _ (If (Expr (Pos.Pos _ (BinaryExpression Eq (Pos.Pos _ (Literal l)) (Pos.Pos _ (Literal r))))) tr fls)) = 
      if l == r 
        then map (snd . Pos.node) (simplBlk tr) 
        else case fls of (Just b) ->  map (snd . Pos.node) (simplBlk b)
                         Nothing  -> []
    simplSS (Pos.Pos x (If e tr fls)) = [Pos.Pos x $ If e tr' fls']
      where
        tr' = simplBlk tr
        fls' =  simplBlk `fmap` fls

    simplSS (Pos.Pos x (While e i b)) = [Pos.Pos x (While e i $ simplBlk b)]
    simplSS x = [x]

emptyModElim :: Program -> Program
emptyModElim (Program decs) = Program (others ++ procs')
  where
    (procs, others) = partition isProc decs
    procs' = map worker procs

    worker (Pos.Pos x (ProcedureDecl name tys formals rets con bod)) = Pos.Pos x $ ProcedureDecl name tys formals rets con' bod
      where
        con' = map updateCon con
        usedVars = fromMaybe Set.empty $ do {(_, blk) <- bod; return $ identsInBlock blk}
        updateCon (Modifies b vs) = Modifies b (filter (`Set.member` usedVars) vs)
        updateCon x = x
    worker x = x


isProc :: Decl -> Bool
isProc (Pos.Pos _ x@ProcedureDecl{}) = True
isProc (Pos.Pos _ x@FunctionDecl{}) = True
isProc _ = False
 
unusedVarElim :: Program -> Program
unusedVarElim (Program decs) = Program (usedOthers ++ procs)
  where
    (procs, others) = partition isProc decs

    isVDecl :: Decl -> Bool
    isVDecl (Pos.Pos _ x@VarDecl{}) = True
    isVDecl _ = False


    allVars = Set.unions $ map identsInDecl procs

    usedOthers = filter (usesVar allVars) others


usesVar :: Set.Set String -> Decl  -> Bool
usesVar vs d = not $ Set.null $ identsInDecl d `Set.intersection` vs

identsInDecl :: Decl -> Set.Set String
identsInDecl (Pos.Pos _ (ConstantDecl _ names _ _ _)) = Set.fromList names
identsInDecl (Pos.Pos _ (VarDecl itws)) = Set.fromList (map itwId itws)
identsInDecl (Pos.Pos _ (AxiomDecl e)) = identsInExpr e
identsInDecl (Pos.Pos _ (ProcedureDecl _ _ _ _ con b)) = bodVs `Set.union` conVs
  where
    conVs = Set.unions $ map worker con
    bodVs = fromMaybe Set.empty $ do {(_, blk) <- b; return $ identsInBlock blk}
    worker (Requires _ e) = identsInExpr e
    worker (Modifies _ vs) = Set.fromList vs
    worker (Ensures _ e) = identsInExpr e
identsInDecl (Pos.Pos _ (FunctionDecl _ _ _ _ _ e)) = maybe Set.empty identsInExpr e

-- | Top-level declaration
-- data BareDecl = 
--   TypeDecl [NewType] |
--   ConstantDecl Bool [Id] Type ParentInfo Bool |                                -- ^ 'ConstantDecl' @unique names type orderSpec complete@
--   FunctionDecl [Attribute] Id [Id] [FArg] FArg (Maybe Expression) |            -- ^ 'FunctionDecl' @name type_args formals ret body@
--   AxiomDecl Expression |
--   VarDecl [IdTypeWhere] |
--   ProcedureDecl Id [Id] [IdTypeWhere] [IdTypeWhere] [Contract] (Maybe Body) |  -- ^ 'ProcedureDecl' @name type_args formals rets contract body@
--   ImplementationDecl Id [Id] [IdType] [IdType] [Body]                          -- ^ 'ImplementationDecl' @name type_args formals rets body@
--   deriving Eq

identsInBlock :: Block -> Set.Set String
identsInBlock blk = Set.unions $ map (worker . Pos.node . snd . Pos.node) blk
  where
    worker :: BareStatement -> Set.Set String
    worker (Assign assns r) = foldl Set.union (Set.fromList $ map fst assns) $ map identsInExpr r
    worker (Predicate _ spec) = identsInExpr $ specExpr spec
    worker (Havoc vs) = Set.fromList vs
    worker (If wce t f) = identsInBlock t `Set.union` maybe Set.empty identsInBlock f `Set.union` wcvs
      where
        wcvs = case wce of 
          Wildcard -> Set.empty
          Expr e -> identsInExpr e
    worker (While wce clauses b) = foldl Set.union wcvs $ identsInBlock b : map (identsInExpr . specExpr) clauses
      where
        wcvs = case wce of 
          Wildcard -> Set.empty
          Expr e -> identsInExpr e
    worker (Call lvs f args) = foldl Set.union (Set.singleton f) $ Set.fromList lvs : map identsInExpr args
    worker x@Break{} = Set.empty
    worker x@Return{} = Set.empty
    worker x@Goto{} = Set.empty
    worker x@Skip{} = Set.empty

  --   data BareStatement = Predicate [Attribute] SpecClause |   -- ^ Predicate statement (assume or assert)
  -- Havoc [Id] |                                            -- ^ 'Havoc' @var_names@
  -- Assign [(Id , [[Expression]])] [Expression] |           -- ^ 'Assign' @var_map_selects rhss@
  -- Call [Id] Id [Expression] |                             -- ^ 'Call' @lhss proc_name args@
  -- CallForall Id [WildcardExpression] |                    -- ^ 'CallForall' @proc_name args@
  -- If WildcardExpression Block (Maybe Block) |             -- ^ 'If' @wild_or_expr then_block else_block@
  -- While WildcardExpression [SpecClause] Block |           -- ^ 'While' @wild_or_expr free_loop_inv loop_body@
  -- Break (Maybe Id) |                                      -- ^ 'Break' @label@
  -- Return |
  -- Goto [Id] |                                             -- ^ 'Goto' @labels@
  -- Skip   


identsInExpr :: Expression -> Set.Set String
identsInExpr = worker . Pos.node
  where
    worker :: BareExpression -> Set.Set String
    worker x@Literal{} = Set.empty
    worker (Var v) = Set.singleton v
    worker x@Logical{} = Set.empty
    worker (Application f args) = foldl Set.union (Set.singleton f) $ map identsInExpr args
    worker (MapSelection l rs) = foldl Set.union (identsInExpr l) $ map identsInExpr rs
    worker (MapUpdate ar is r) = foldl Set.union (identsInExpr ar `Set.union` identsInExpr r) $ map identsInExpr is
    worker (Old e) = identsInExpr e
    worker (IfExpr c t f) = identsInExpr c `Set.union` identsInExpr t `Set.union` identsInExpr f
    worker (Coercion e _) = identsInExpr e
    worker (UnaryExpression _ e) = identsInExpr e
    worker (BinaryExpression _ l r) = identsInExpr l `Set.union` identsInExpr r
    worker (Quantified _ _ _ i) = identsInExpr i


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
--   deriving (Eq, Ord, Data, Typeable)  -- syntactic equality
  