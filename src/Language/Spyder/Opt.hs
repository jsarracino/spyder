module Language.Spyder.Opt (
    constFolding
  , optimize
  , deadCodeElim
  , gatherConstDecs
)

where

import Language.Boogie.AST
import qualified Data.Map.Strict as Map
import qualified Language.Boogie.Position as Pos
import Language.Spyder.Translate.Direct
import Data.Maybe()

optimize :: Program -> Program
optimize p@(Program decs) = doAllOpts p 
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
    simpl (Pos.Pos x (ProcedureDecl nme tys args rets con (Just bod))) = Pos.Pos x (ProcedureDecl nme tys args rets con (Just $ bod'))
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
    simplSS x = [x]

