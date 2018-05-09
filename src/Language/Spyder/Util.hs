module Language.Spyder.Util (
    strip
  , prefix
  , incSpan
  , flat
  , breadthFirst
  , allocateFreshVar
) where

-- import Data.List
import Control.Monad (join)
import Language.Boogie.AST      (Decl(..))

--- prefix x y: is x a prefix of y?
prefix :: String -> String -> Bool
prefix (x:xs) (y:ys) = if (x == y) then prefix xs ys else False
prefix [] _ = True

-- strip x y: the suffix of y w.r.t to x, i.e. y with the first x elements removed
strip :: String -> String -> String
strip (x:xs) (y:ys) = if (x == y) then strip xs ys else y:ys
strip [] r = r
strip _ [] = []

-- inclusive span: incSpan p a -> (trus, fls), where trus is all trus + the first element that
-- is false
incSpan :: (a -> Bool) -> [a] -> ([a], [a])
incSpan f inp = (pre ++ l, post)
  where (pre, post') = span f inp
        (l, post) = case post' of
          (x:xs) -> ([x], xs)
          [] -> ([], [])

-- take a list and flatten it. heck.
flat :: [[a]] -> [a]
flat = join


-- take a list of lists and collect the elements in breadth-first order
-- e.g. [[1,2,3], [5,6]] => [[1,5], [2,5], [3,5], [1,6], [2,6], [3,6]]
breadthFirst :: [[a]] -> [[a]]
breadthFirst = foldl worker [[]]
  where worker acc xs = do {x <- xs; a <- acc; return $ a ++ [x]}
-- let foo xs ys = do {x <- xs; y <- ys; return $ x:y}

allocateFreshVar :: [Decl] -> Maybe String -> String
allocateFreshVar decs suffix = makeVar finalVar
  where
    makeVar = undefined "TODO"
    finalVar = undefined "TODO"


-- TypeDecl [NewType] |
-- ConstantDecl Bool [Id] Type ParentInfo Bool |                                -- ^ 'ConstantDecl' @unique names type orderSpec complete@
-- FunctionDecl [Attribute] Id [Id] [FArg] FArg (Maybe Expression) |            -- ^ 'FunctionDecl' @name type_args formals ret body@
-- AxiomDecl Expression |
-- VarDecl [IdTypeWhere] |
-- ProcedureDecl Id [Id] [IdTypeWhere] [IdTypeWhere] [Contract] (Maybe Body) |  -- ^ 'ProcedureDecl' @name type_args formals rets contract body@
-- ImplementationDecl Id [Id] [IdType] [IdType] [Body]                          -- ^ 'ImplementationDecl' @name type_args formals rets body@