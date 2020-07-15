module Language.Spyder.Parser.Parser (
    expr
  , stmt
  , prog
  , block
  , relDeclP
  , relP
  , spaced
  , typ
  , dataDeclP
  , loopP
  , elifP
  , condP
  , relPrev
  , specTerm
) where

import Text.Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Token as Tok
import Language.Spyder.Parser.Lexer         (spyderLexer, Parser)
import Language.Spyder.AST.Component
import qualified Language.Spyder.AST.Imp  as Imp
import qualified Language.Spyder.AST.Spec as Spec
import Language.Spyder.AST                  (Program(..), Procedure(..))

import Control.Monad                        (liftM, liftM2)
import Data.List                            (partition)

import Data.Maybe                           (catMaybes)

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

-- arrAccess = do {
--   pref <- liftM Imp.VConst ident;
--   rhs <- many1 $ brackets expr;
--   return $ foldl Imp.Index pref rhs
-- }
-- arrPrim = liftM Imp.AConst (brackets $ commas expr)

exprTerm :: Parser Imp.Expr
exprTerm    =  parens expr
  <|> try (liftM Imp.IConst ints)
  <|> try (liftM Imp.BConst bools)
  -- <|> try arrPrim
  -- <|> try arrAccess
  <|> try (liftM Imp.VConst ident)
  <?> "simple expr"


relPrev :: Parser Spec.RelExpr
relPrev = do {
  res "prev";
  (v, ifFls) <- parens prevPair;
  return $ Spec.Prev v ifFls
} where
  prevPair = do {
    nme <- ident;
    comma;
    tl <- relexpr;
    return (nme, tl)
  }

relApp :: Parser Spec.RelExpr
relApp = do {
  nme <- ident;
  args <- parens $ commas relexpr;
  return $ Spec.RelApp nme args
}
relForeach :: Parser Spec.RelExpr
relForeach = do {
  res "foreach";
  binds <- bindings;
  spaces;
  res "with";
  spaces;
  idx <- loopIdxP;
  spaces;
  body <- braces relexpr;
  pure $ Spec.Foreach (fst $ unzip binds) idx (snd $ unzip binds) body
}

specTerm :: Parser Spec.RelExpr
specTerm  =  parens relexpr
  <|> try (liftM Spec.RelInt ints)
  <|> try (liftM Spec.RelBool bools)
  <|> try relForeach
  <|> try relPrev
  -- <|> try relIndex
  <|> try relApp
  <|> try (liftM Spec.RelVar ident)
  <?> "spec expr"

-- relIndex = do {
--   pref <- liftM Spec.RelVar ident;
--   rhs <- many1 $ brackets relexpr;
--   return $ foldl Spec.RelIndex pref rhs
-- }

expr :: Parser Imp.Expr
expr = buildExpressionParser exprTable exprTerm <?> "expression"
  
relexpr :: Parser Spec.RelExpr
relexpr = buildExpressionParser relTable specTerm <?> "relation expression"

exprTable   = [ 
      [unary "!" (Imp.UnOp Imp.Not), unary "-" (Imp.UnOp Imp.Neg)]
  ,   [binary "*" (Imp.BinOp Imp.Mul) AssocLeft, binary "/" (Imp.BinOp Imp.Div) AssocLeft ]
  ,   [binary "+" (Imp.BinOp Imp.Plus) AssocLeft, binary "-" (Imp.BinOp Imp.Minus) AssocLeft ]
  ,   [binary "%" (Imp.BinOp Imp.Mod) AssocLeft]
  ,   [
        binary "<" (Imp.BinOp Imp.Lt) AssocLeft, binary "<=" (Imp.BinOp Imp.Le) AssocLeft
      , binary ">" (Imp.BinOp Imp.Gt) AssocLeft, binary ">=" (Imp.BinOp Imp.Ge) AssocLeft
    ]
  ,   [binary "=" (Imp.BinOp Imp.Eq) AssocLeft, binary "!=" (Imp.BinOp Imp.Neq) AssocLeft]
  ,   [binary "&&" (Imp.BinOp Imp.And) AssocLeft, binary "||" (Imp.BinOp Imp.Or) AssocLeft]
 ]

relTable   = [
      [unary "!" (Spec.RelUnop Spec.Not), unary "-" (Spec.RelUnop Spec.Neg)]
  ,   [binary "*" (Spec.RelBinop Spec.Mul) AssocLeft, binary "/" (Spec.RelBinop Spec.Div) AssocLeft ]
  ,   [binary "%" (Spec.RelBinop Spec.Mod) AssocLeft]
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
  binds <- bindings;
  idx <- loopIdxP;
  body <- braces block;
  return $ Imp.For binds idx body
}

commas1 :: Parser a -> Parser [a]
commas1 p = do {
  spaces;
  h <- p;
  spaces;
  t <- many (p <* comma);
  pure $ h : t
}
bindings :: Parser [(String, String)]
bindings = commas1 $ do {
  v <- ident;
  spaces;
  res "in";
  spaces;
  r <- ident;
  pure (v, r)
}

-- whileP :: Parser Imp.Statement
-- whileP = do {
--   res "while";
--   cond <- parens expr;
--   body <- braces block;
--   return $ Imp.While cond body
-- }

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
  -- <|> try whileP
  <|> try condP
  <|> try (liftM2 Imp.Assgn ident ((symb "=" >> expr) `followedBy` semi))
  <?> "Statement"

dataDeclP :: Parser Imp.VDecl
dataDeclP = res "data" >> vdecl 
  
relP :: Parser Spec.RelExpr 
relP = relexpr


relDeclP :: Parser Spec.RelExpr
relDeclP = do {
  res "invariant";
  spaces;
  relP;
}


procP :: Parser Procedure
procP = do {
  res "procedure";
  name <- ident;
  args <- parens $ commas vdecl;
  bod <- braces block;
  return $ Proc name args bod
}

muncher :: Parser a -> Parser String
muncher end = manyTill anyChar end

munchOne :: Parser a -> Parser b -> Parser (Maybe a)
munchOne it end = 
      ((try it) >>= pure . Just)
  <|> ((muncher end) >>= (\_ -> pure Nothing))

data ProgInter = L Imp.VDecl | M Spec.RelExpr | R Procedure

progOne :: Parser (Maybe ProgInter)
progOne = munchOne worker (res ";;")
  where
    worker = ddp <|> rp <|> pp
    ddp = do {
      x <- dataDeclP;
      pure $ L x
    }
    rp = do {
      x <- relDeclP;
      pure $ M x
    }
    pp = do {
      x <- procP;
      pure $ R x
    }

progAll :: Parser [ProgInter]
progAll = liftM catMaybes (many1 progOne)

prog :: Parser Program
prog = liftM worker progAll
  where
    worker :: [ProgInter] -> Program
    worker = foldl folder (Program [] [] [])
    folder :: Program -> ProgInter -> Program
    folder p (L x) = Program (x : vars p) (invs p) (procs p)
    folder p (M x) = Program (vars p) (x : invs p) (procs p)
    folder p (R x) = Program (vars p) (invs p) (x : procs p)
