module Language.Spyder.Bench (
    boogSize
  , spySize
  , invSize
)

where

import Language.Spyder.AST.Spec
import Language.Spyder.AST
import qualified Language.Boogie.AST as BST
import Language.Spyder.AST.Component
import qualified Language.Spyder.AST.Imp as Imp

-- don't use this; instead, diff at command line
boogSize :: BST.Program -> Int
boogSize = error "don't use me"

-- size of spyder ast (main less using + relations in components)
spySize :: Program -> Int
spySize (others, main) = size main + sum (map size others)
size (MainComp decs) = sum $ map mdec decs
size (DerivComp _ decs) = sum $ map ddec decs


mdec (MainDDecl vd) = 1 + vsize vd
mdec (MainUD _ ) = 0
mdec (ProcDecl _ fs (Imp.Seq ss)) = 1 + sum (map vsize fs) + sum (map ssize ss)

ssize (Imp.Decl vd (Just e)) = 1 + vsize vd + esize e
ssize (Imp.Decl vd Nothing) = 1 + vsize vd
ssize (Imp.Assgn _ e) = 1 + esize e
ssize (Imp.For vs (Just _) arrs (Imp.Seq ss)) = 2 + sum (map vsize vs) + length arrs + sum (map ssize ss)
ssize (Imp.For vs Nothing arrs (Imp.Seq ss)) = 1 + sum (map vsize vs) + length arrs + sum (map ssize ss)
ssize (Imp.Cond c (Imp.Seq st) (Imp.Seq sf)) = 1 + esize c + sum (map ssize $ st ++ sf) 

esize (Imp.Index l r) = 1 + esize l + esize r
esize (Imp.UnOp _ i) = 1 + esize i
esize (Imp.BinOp _ l r) = 1 + esize l + esize r
esize Imp.VConst{} = 1
esize Imp.IConst{} = 1
esize Imp.BConst{} = 1


vsize (_,t) = 1 + tsize t
tsize (Imp.ArrTy i) = tsize i + 1
tsize _ = 1


ddec (RelDecl _ _ i) = isize i
ddec _ = 0
isize (Foreach ls (Just s) as b) = length (ls ++ as) + 2 + isize b
isize (Foreach ls Nothing as b) = length (ls ++ as) + 1 + isize b
isize (Prev s i) = 2 + isize i
isize (RelUnop _ i) = 1 + isize i
isize (RelBinop _ l r ) = isize l + isize r + 1
-- isize (RelIndex l r ) = isize l + isize r + 1
isize RelVar{} = 1
isize RelInt{} = 1
isize RelBool{} = 1





-- count of ast nodes for invariant
invSize :: Program -> Int
invSize (others, _) = sum (map size others)