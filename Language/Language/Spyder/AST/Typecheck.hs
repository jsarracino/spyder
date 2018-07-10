module Language.Spyder.AST.Typecheck (
    -- typecheck
  -- , 
) where

-- import qualified Data.Map.Strict as Map

-- type TypeContext = Map String a

-- class Typeable a where
--   typecheck :: a -> Maybe Type
--   intTy :: a -> Bool
--   intTy x = case typecheck x of 
--     (Just (BaseTy "int")) -> True
--     _                     -> False

--   -- VConst String       -- Variables
--   -- | IConst Int      -- Integers
--   -- -- | SConst String       -- Strings
--   -- | BConst Bool         -- Booleans
--   -- | AConst [Expr]       -- Arrays
--   -- | BinOp Bop Expr Expr -- Binary operations
--   -- | UnOp Uop Expr       -- Unary operations
--   -- | Index Expr Expr     -- Array indexing e.g. foo[bar]
--   -- | App Expr [Expr]       -- function calls (not procedure calls)
-- instance Typeable Expr where
--   typecheck IConst{} = Just $ BaseTy "int"
--   typecheck VConst{} = Just $ BaseTy "int"
--   typecheck BConst{} = Just $ BaseTy "bool"
--   typecheck (BinOp o l r) = case (typecheck l, typecheck r) of
--     (Just lt, Just rt) -> if lt == rt then checkOp lt else Nothing
--     _                  -> Nothing
--     where 
--       checkOp ty
--         | o `elem` [Plus, Minus, Mul, Div] && ty == BaseTy "int"      = Just ty
--         | o `elem` [Lt, Gt, Le, Ge , And, Or] && ty == BaseTy "bool"  = Just ty
--         | o `elem` [Eq, Neq]                                          = Just $ BaseTy "bool"
--         | otherwise                                                   = Nothing
--   typecheck (UnOp o i) = case (o, typecheck i) of
--     (Neg, Just (BaseTy "int"))  -> Just $ BaseTy "bool"
--     (Not, Just (BaseTy "bool")) -> Just $ BaseTy "bool"
--     _                           -> Nothing
--   typecheck _ = Nothing -- TODO: fix varconst hack, app, index, aconst