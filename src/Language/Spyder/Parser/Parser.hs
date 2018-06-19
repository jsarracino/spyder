module Language.Spyder.Parser.Parser (
    expr
  , stmt
  , prog
  , block
  , relDeclP
  , relP
  , comp
  , spaced
  , typ
  , dataDeclP
  , mainCompP
  , derivCompP
  , loopP
  , elifP
  , condP
) where

import Text.Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Token as Tok
import Language.Spyder.Parser.Lexer         (spyderLexer, Parser)
import Language.Spyder.AST.Component
import qualified Language.Spyder.AST.Imp  as Imp
import qualified Language.Spyder.AST.Spec as Spec
import Language.Spyder.AST                  (Program)

import Control.Monad                        (liftM, liftM2)
import Data.List                            (partition)

import Language.Boogie.Parser as BP
-- import

lexer = spyderLexer
res = Tok.reserved lexer
parens = Tok.parens lexer
symb = Tok.symbol lexer
ident = Tok.identifier lexer
semi = Tok.semi lexer
comma = Tok.comma lexer
commas = Tok.commaSep lexer
braces = Tok.braces lexer
brackets = Tok.brackets lexer

followedBy :: Parser a -> Parser b -> Parser a
followedBy p q = do {r <- p; q; return r}

spaced :: Parser a -> Parser [a]
spaced p = p `sepBy` Tok.whiteSpace lexer
semis p = p `sepBy` semi

ints = liftM fromIntegral $ Tok.integer lexer 
bools = tru <|> fls
  where
    tru = do {res "true"; return True }
    fls = do {res "false"; return False }

arrAccess = do {
  pref <- liftM Imp.VConst ident;
  rhs <- many1 $ brackets expr;
  return $ foldl Imp.Index pref rhs
}
arrPrim = liftM Imp.AConst (brackets $ commas expr)

exprTerm :: Parser Imp.Expr
exprTerm    =  parens expr
  <|> try (liftM Imp.IConst ints)
  <|> try (liftM Imp.BConst bools)
  <|> try arrPrim
  <|> try arrAccess
  <|> try (liftM Imp.VConst ident)
  <?> "simple expr"


relApp :: Parser Spec.RelExpr
relApp = do {
  nme <- ident;
  args <- parens $ commas relexpr;
  return $ Spec.RelApp nme args
}
relForeach :: Parser Spec.RelExpr
relForeach = do {
  res "foreach";
  vs <- parens $ commas ident;
  res "in";
  arrs <- parens $ commas ident;
  body <- braces relexpr;
  return $ Spec.Foreach vs arrs body
}
specTerm :: Parser Spec.RelExpr
specTerm  =  parens relexpr
  <|> try (liftM Spec.RelInt ints)
  <|> try (liftM Spec.RelBool bools)
  <|> try relApp
  <|> try relForeach
  <|> try (liftM Spec.RelVar ident)
  <?> "spec expr"

expr :: Parser Imp.Expr
expr = buildExpressionParser exprTable exprTerm <?> "expression"
  
relexpr :: Parser Spec.RelExpr
relexpr = buildExpressionParser relTable specTerm <?> "relation expression"

exprTable   = [ 
      [unary "!" (Imp.UnOp Imp.Not), unary "-" (Imp.UnOp Imp.Neg)]
  ,   [binary "*" (Imp.BinOp Imp.Mul) AssocLeft, binary "/" (Imp.BinOp Imp.Div) AssocLeft ]
  ,   [binary "+" (Imp.BinOp Imp.Plus) AssocLeft, binary "-" (Imp.BinOp Imp.Minus) AssocLeft ]
  ,   [
        binary "<" (Imp.BinOp Imp.Lt) AssocLeft, binary "<=" (Imp.BinOp Imp.Le) AssocLeft
      , binary ">" (Imp.BinOp Imp.Gt) AssocLeft, binary ">=" (Imp.BinOp Imp.Ge) AssocLeft
    ]
  ,   [binary "==" (Imp.BinOp Imp.Eq) AssocLeft, binary "!=" (Imp.BinOp Imp.Neq) AssocLeft]
  ,   [binary "&&" (Imp.BinOp Imp.And) AssocLeft, binary "||" (Imp.BinOp Imp.Or) AssocLeft]
 ]

relTable   = [
      [unary "!" (Spec.RelUnop Spec.Not), unary "-" (Spec.RelUnop Spec.Neg)]
  ,   [binary "*" (Spec.RelBinop Spec.Mul) AssocLeft, binary "/" (Spec.RelBinop Spec.Div) AssocLeft ]
  ,   [binary "+" (Spec.RelBinop Spec.Plus) AssocLeft, binary "-" (Spec.RelBinop Spec.Minus) AssocLeft ]
  ,   [
        binary "<" (Spec.RelBinop Spec.Lt) AssocLeft, binary "<=" (Spec.RelBinop Spec.Le) AssocLeft
      , binary ">" (Spec.RelBinop Spec.Gt) AssocLeft, binary ">=" (Spec.RelBinop Spec.Ge) AssocLeft
    ]
  ,   [binary "=" (Spec.RelBinop Spec.Eq) AssocLeft, binary "!=" (Spec.RelBinop Spec.Neq) AssocLeft]
  ,   [binary "&&" (Spec.RelBinop Spec.And) AssocLeft, binary "||" (Spec.RelBinop Spec.Or) AssocLeft]
  ,   [binary "==>" (Spec.RelBinop Spec.Imp) AssocLeft, binary "<=>" (Spec.RelBinop Spec.Iff) AssocLeft]
  ]

binary name fun = Infix (do{ Tok.reservedOp lexer name; return fun })
unary name fun  = Prefix (do{ Tok.reservedOp lexer name; return fun })

baseTy :: Parser Imp.Type
baseTy = 
      try (res "int" >> return Imp.IntTy)
  <|> try (res "bool" >> return Imp.BoolTy)
  <?> "Base Type"

typ :: Parser Imp.Type
typ =
      try arrTy
  <|> baseTy
  <?> "Type"

arrTy :: Parser Imp.Type
arrTy = do {
  h <- baseTy;
  arrs <- many1 (symb "[]");
  return $ foldl (\x s -> Imp.ArrTy x) h arrs
}

vdecl :: Parser Imp.VDecl
vdecl = do {
  vname <- ident;
  symb ":";
  ty <- typ;
  return (vname, ty)
}

loopIdxP :: Parser (Maybe String)
loopIdxP = optionMaybe $ res "with" >> ident

loopP :: Parser Imp.Statement
loopP = do {
  res "for";
  vs <- parens $ commas vdecl;
  idx <- loopIdxP;
  res "in";
  arrs <- parens $ commas expr;
  body <- braces block;
  return $ Imp.For vs idx arrs body
}

whileP :: Parser Imp.Statement
whileP = do {
  res "while";
  cond <- parens expr;
  body <- braces block;
  return $ Imp.While cond body
}

declP :: Parser Imp.Statement
declP = do {
  res "let";
  vname <- vdecl;
  rhs <- optionMaybe $ try $ symb "=" >> expr;
  semi;
  return $ Imp.Decl vname rhs
}

condP :: Parser Imp.Statement
condP = do {
  res "if";
  cond <- parens expr;
  tru <- braces block;
  fls <- option (Imp.Seq []) tail;
  return $ Imp.Cond cond tru fls;
} where
    tail = try (res "else" >> braces block) <|> elifP

elifP :: Parser Imp.Block
elifP = do {
  res "else";
  cond <- condP;
  return $ Imp.Seq [cond];
}


block :: Parser Imp.Block
block = liftM Imp.Seq (spaced stmt)
  <?> "Block"

stmt :: Parser Imp.Statement
stmt =
      try declP
  <|> try loopP
  <|> try whileP
  <|> try condP
  <|> try (liftM2 Imp.Assgn expr ((symb "=" >> expr) `followedBy` semi))
  <?> "Statement"

derivCompP :: Parser DerivDecl
derivCompP = 
      try (liftM DeriveDDecl dataDeclP)
  <|> try relDeclP
  <|> try alwaysP
  <?> "Component Member"


alwaysP :: Parser DerivDecl
alwaysP = res "always" >> liftM InvClaus relP `followedBy` semi

usingP :: Parser UseClause
usingP = do {
  res "using";
  cname <- ident;
  args <- parens $ ident `sepBy` comma;
  semi;
  return (cname, args )
}

dataDeclP :: Parser Imp.VDecl
dataDeclP = res "data" >> vdecl `followedBy` semi
  

relP :: Parser Spec.RelExpr 
relP = relexpr

mainCompP :: Parser MainDecl
mainCompP = 
      try (liftM MainDDecl dataDeclP)
  <|> try procP
  <|> try (liftM MainUD usingP)
  <?> "Main decl parser"

relDeclP :: Parser DerivDecl
relDeclP = do {
  res "relation";
  name <- ident;
  formals <- parens $ commas vdecl;
  bod <- braces relP;
  return $ RelDecl name formals bod;
}


procP :: Parser MainDecl
procP = do {
  res "procedure";
  name <- ident;
  args <- parens $ commas vdecl;
  bod <- braces block;
  return $ ProcDecl name args bod
}

comp :: Parser Component
comp = do {
  res "Component";
  name <- ident;
  if name=="Main" then do {
    decls <- braces $ spaced mainCompP;
    return $ MainComp decls
  } else do {
    decls <- braces $ spaced derivCompP;
    return $ DerivComp name decls
  }
}





prog :: Parser Program
prog = do {
  comps <- spaced comp;
  let ([it], others) = partition takeMain comps in 
    return (others, it)
} where
    takeMain MainComp{} = True
    takeMain _ = False
