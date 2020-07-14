module Language.Spyder.Translate.Desugar (
    stripLt
  , gatherDecls
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
    stmtWorker (Assgn l r) = Assgn l (exprWorker r)
    stmtWorker (For vs idx arrs bod) = For vs idx (map exprWorker arrs) (stripLt bod)
    stmtWorker (Cond c tr fl) = Cond (exprWorker c) (stripLt tr) (stripLt fl)
    exprWorker x@(VConst _) = x
    exprWorker x@(IConst _) = x
    exprWorker x@(BConst _) = x
    exprWorker (UnOp o i) = UnOp o $ exprWorker i
    exprWorker (BinOp Lt l r) = UnOp Not $ BinOp Ge l r
    exprWorker (BinOp o l r) = BinOp o (exprWorker l) (exprWorker r)

-- gather decls 
gatherDecls :: Block -> Set VDecl
gatherDecls (Seq ss) = foldl union empty (map stmtWorker ss)
  where
    stmtWorker (Decl d _) = singleton d
    stmtWorker (For _ _ _ bod) = gatherDecls bod 
    -- stmtWorker (While _ bod) = gatherDecls bod
    stmtWorker (Assgn _ _) = empty
    stmtWorker (Cond _ tr fl) = gatherDecls tr `union` gatherDecls fl

generateBoogieBlock :: Block -> [Statement]
generateBoogieBlock b =
  let (Seq ret) = stripLt b in ret

  