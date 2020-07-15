
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}


module Language.Spyder.Pretty (
  pretty
) where

import Language.Spyder.AST
import Language.Spyder.AST.Imp
import qualified Language.Spyder.AST.Spec as Spec
import Data.String

import Text.PrettyPrint.ANSI.Leijen
import Text.PrettyPrint.ANSI.Leijen.Internal

-- instance IsString Doc where
--   fromString = text

instance Pretty Expr where
  pretty (VConst x) = text x
  pretty (IConst x) = int x
  pretty (BConst x) = if x then "true" else "false"
  pretty (BinOp o l r) = parens (pretty l <+> pretty o <+> pretty r)
  pretty (UnOp o x) = pretty o <> parens (pretty x)

instance Pretty Uop where
  pretty Neg = "-"
  pretty Not = "!"

instance Pretty Bop where
  pretty Plus = "+"
  pretty Minus = "-"
  pretty Mul = "*"
  pretty Div = "/"
  pretty Lt = "<"
  pretty Gt = ">"
  pretty Le = "<="
  pretty Ge = ">="
  pretty And = "&&"
  pretty Or = "||"
  pretty Eq = "="
  pretty Neq = "!="
  pretty Mod = "%"

instance Pretty Type where
  pretty IntTy = "int"
  pretty BoolTy = "bool"
  pretty (ArrTy x) = let (base, end) = worker empty x in pretty base <> end
    where
      worker acc ty = case ty of 
        IntTy  -> (ty, acc)
        BoolTy -> (ty, acc)
        (ArrTy t) -> worker (acc <> "[]") t

pretty' :: VDecl -> Doc
pretty' (x, ty) = text x <> text ":" <+> pretty ty

commaSep :: [Doc] -> Doc
commaSep ds = hsep $ punctuate comma ds

instance Pretty Statement where
  pretty (Decl vd me) = let pref = text "let" <+> pretty' vd in case me of 
    Just init -> pref <+> text "=" <+> pretty init <> semi
    Nothing -> pref <> semi
  pretty (Assgn l r) = text l <+> text "=" <+> pretty r <> semi
  pretty (Cond e tb fb) = text "if" <+> parens (pretty e) <+> pretty tb <+> pretty fb
  pretty (For binds midx bod) = let pref = text "for" <+> commaSep (map fmtBind binds) in
    case midx of 
      Just v -> pref <+> text "with" <+> text v <+> pretty bod
      Nothing -> pref <+> pretty bod

fmtBind :: (String, String) -> Doc
fmtBind (v, a) = text v <+> text "in" <+> text a

instance Pretty Block where
  pretty (Seq ss) = nest 2 $ vcat (map pretty ss) 

instance Pretty Procedure where
  pretty (Proc nme formls bod) = 
    text "procedure" <+> text nme <> parens (commaSep $ map pretty' formls) <+> braces (pretty bod) <> semi <> semi

instance Pretty Spec.RelBop where
  pretty Spec.Plus = "+"
  pretty Spec.Minus = "-"
  pretty Spec.Mul = "*"
  pretty Spec.Div = "/"
  pretty Spec.Lt = "<"
  pretty Spec.Gt = ">"
  pretty Spec.Le = "<="
  pretty Spec.Ge = ">="
  pretty Spec.And = "&&"
  pretty Spec.Or = "||"
  pretty Spec.Eq = "="
  pretty Spec.Neq = "!="
  pretty Spec.Mod = "%"
  pretty Spec.Imp = "==>"
  pretty Spec.Iff = "<=>"

instance Pretty Spec.RelUop where
  pretty Spec.Neg = "-"
  pretty Spec.Not = "!"

instance Pretty Spec.RelExpr where
  pretty (Spec.RelVar x) = text x
  pretty (Spec.RelInt x) = int x
  pretty (Spec.RelBool x) = if x then "true" else "false"
  pretty (Spec.RelBinop o l r) = parens (pretty l <+> pretty o <+> pretty r)
  pretty (Spec.RelUnop o x) = pretty o <> parens (pretty x)
  pretty Spec.RelApp{} = error "unsupported"
  pretty (Spec.Foreach vs midx arrs b) = let pref = text "foreach" <+> commaSep (map fmtBind binds) in
    case midx of 
      Just v -> pref <+> text "with" <+> text v <+> nest 2 (pretty b)
      Nothing -> pref <+> nest 2 (pretty b)
    where
      binds = zip vs arrs
  pretty (Spec.RelCond c tb fb) = parens ("if" <+> pretty c <+> "then" <+> pretty tb <+> "else" <+> pretty fb)
  pretty (Spec.Prev v emp) = "prev" <> parens (text v <> comma <> pretty emp)


instance (Pretty Program) where
  pretty (Program vdecs invs procs) = vcat $ map dpretty vdecs ++ map ipretty invs ++ map pretty procs
    where
      dpretty vdec = text "data" <+> pretty vdec <> semi <> semi
      ipretty inv = text "invariant" <+> pretty inv <> semi <> semi
