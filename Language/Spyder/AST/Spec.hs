module Language.Spyder.AST.Spec (
  RelExpr(..)
  , RelBop(..)
  , RelUop(..)
  , weakenPrev
  , containsPrev
  , inlinePrev
) where


-- basically, the binary expression grammar from Imp, as well as foreaches. 
-- TODO: figure out how to refactor. 
data RelBop =
    Plus | Minus | Mul | Div
  | Lt | Gt | Le | Ge  | And | Or | Eq | Neq
  | Imp | Iff | Mod
  deriving (Eq, Show, Ord)

data RelUop =
  Neg | Not
  deriving (Eq, Show, Ord)

data RelExpr =
    RelVar String       -- Variables
  | RelInt Int      -- Integers
  | RelBool Bool         -- Booleans
  -- | Deref RelExpr       -- 
  -- | RelIndex RelExpr RelExpr -- index
  | RelBinop RelBop RelExpr RelExpr -- Binary operations
  | RelUnop RelUop RelExpr       -- Unary operations
  | RelApp String [RelExpr]       -- function calls (not procedure calls)
  | Foreach [String] (Maybe String) [String] RelExpr  -- foreach (x,y,z) [with idx] in (p, q,s) {<expr>}
  | Prev String RelExpr -- prev(var, ifEmpty)
  | RelCond RelExpr RelExpr RelExpr -- wo
  deriving (Eq, Show, Ord)


containsPrev :: RelExpr -> Bool
containsPrev x@Prev{} = True
containsPrev (RelBinop _ l r) = containsPrev l || containsPrev r
containsPrev (RelUnop _ i) = containsPrev i
containsPrev (Foreach _ _ _ b) = containsPrev b
containsPrev (RelApp "prev" _) =True
containsPrev (RelApp "prev_var" _) = True
containsPrev _ = False


weakenPrev :: RelExpr -> RelExpr
weakenPrev (Foreach vs i ars b) = Foreach vs i ars $ weakenPrev b
weakenPrev (RelBinop And l r) = RelBinop And (weakenPrev l) (weakenPrev r)
-- numeric ops
weakenPrev x@(RelBinop o l r) = if containsPrev x then RelBool True else RelBinop o l r
weakenPrev (RelUnop o i) = RelUnop o $ weakenPrev i
weakenPrev (Prev _ _) = RelBool True
weakenPrev (RelApp "prev" _) = RelBool True
weakenPrev (RelApp "prev_var" _) = RelBool True
weakenPrev x = x -- TODO: maybe app


-- unfortunately we have to inline prev to make boogaloo succeed by command-line calls
inlinePrev :: RelExpr -> RelExpr
inlinePrev x@Prev{}= error "prev should be desugared into app"
inlinePrev (RelBinop o l r) = RelBinop o (inlinePrev l) (inlinePrev r)
inlinePrev (RelUnop o i) = RelUnop o $ inlinePrev i
inlinePrev x@(Foreach vs i ars b) = x -- Foreach vs i ars $ inlinePrev b
inlinePrev (RelApp "prev" [v, base, idx]) = RelCond (RelBinop Eq idx (RelInt 0)) base v 
inlinePrev (RelApp "prev_var" [v, base, idx]) = RelCond (RelBinop Eq idx (RelInt 0)) base v 
inlinePrev x = x

-- RelApp "prev_var" [RelVar ("prev_" ++ v), i, RelVar idx]