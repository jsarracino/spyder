module Language.Spyder.Parser.Parser (
    expr
  , stmt
  , prog
  , block
  , relDeclP
  , relP
  , comp
  , spaced
) where

import Text.Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Token as Tok
import Language.Spyder.Parser.Lexer         (spyderLexer, Parser)
import Language.Spyder.AST.Component
import Language.Spyder.AST.Imp
import Language.Spyder.AST.Spec
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

ints = liftM (IConst . fromIntegral) $ Tok.integer lexer
vars = liftM VConst ident
bools = tru <|> fls
  where
    tru = do {res "true"; return $ BConst True }
    fls = do {res "false"; return $ BConst False }

arrAccess = do {
  pref <- vars;
  rhs <- many1 $ brackets expr;
  return $ foldl Index pref rhs
}
arrPrim = liftM AConst (brackets $ commas expr)

 -- TODO:
 -- | UnOP Uop Expr
term :: Parser Expr
term    =  parens expr
  <|> try ints
  <|> try bools
  <|> try arrPrim
  <|> try arrAccess
  <|> vars
  <?> "simple expr"

-- for now, just identifiers
-- TODO: these are both hacks and should be fixed!!!

expr :: Parser Expr
expr    = buildExpressionParser exprTable term
         <?> "expression"

exprTable   = [
    [binary "*" (BinOp Mul) AssocLeft, binary "/" (BinOp Div) AssocLeft ]
 ,  [binary "+" (BinOp Plus) AssocLeft, binary "-" (BinOp Minus) AssocLeft ]
 ,  [
      binary "<" (BinOp Lt) AssocLeft, binary "<=" (BinOp Le) AssocLeft
    , binary ">" (BinOp Gt) AssocLeft, binary ">=" (BinOp Ge) AssocLeft
  ]
 ,  [binary "==" (BinOp Eq) AssocLeft, binary "!=" (BinOp Neq) AssocLeft]
 ,  [binary "&&" (BinOp And) AssocLeft, binary "||" (BinOp Or) AssocLeft]
 ]

binary name fun =
  Infix (do{ Tok.reservedOp lexer name; return fun })


typ :: Parser Type
typ =
      try arrTy
  <|> liftM BaseTy ident
  <?> "Type"

arrTy :: Parser Type
arrTy = do {
  h <- liftM BaseTy ident;
  arrs <- many1 (symb "[]");
  return $ foldl (\x s -> ArrTy x) h arrs
}

vdecl :: Parser VDecl
vdecl = do {
  vname <- ident;
  symb ":";
  ty <- typ;
  return (vname, ty)
}


loopP :: Parser Statement
loopP = do {
  res "for";
  vs <- parens $ commas vdecl;
  res "in";
  arrs <- parens $ commas expr;
  body <- braces block;
  return $ Loop vs arrs body
}

whileP :: Parser Statement
whileP = do {
  res "while";
  cond <- parens expr;
  body <- braces block;
  return $ While cond body
}

declP :: Parser Statement
declP = do {
  res "let";
  vname <- vdecl;
  rhs <- optionMaybe $ try $ symb "=" >> expr;
  semi;
  return $ Decl vname rhs
}

condP :: Parser Statement
condP = do {
  res "if";
  cond <- parens expr;
  tru <- braces block;
  fls <- option (Seq []) tail;
  return $ Cond cond tru fls;
} where
    tail = try (res "else" >> braces block)

block :: Parser Block
block = liftM Seq (spaced stmt)
  <?> "Block"

stmt :: Parser Statement
stmt =
      try declP
  <|> try loopP
  <|> try whileP
  <|> try condP
  <|> try (liftM2 Assgn expr ((symb "=" >> expr) `followedBy` semi))
  <?> "Statement"

derivCompP :: Parser DerivDecl
derivCompP = 
      try (liftM DeriveDDecl dataDeclP)
  <|> try relDeclP
  <|> try alwaysP
  <?> "Component Member"


alwaysP :: Parser DerivDecl
alwaysP = res "always" >> liftM (InvClaus . BE) BP.expression `followedBy` semi

usingP :: Parser UseClause
usingP = do {
  res "using";
  cname <- ident;
  args <- parens $ ident `sepBy` comma;
  semi;
  return (cname, args )
}

dataDeclP :: Parser VDecl
dataDeclP = res "data" >> vdecl `followedBy` semi
  

relP :: Parser BaseRel 
relP = liftM injectForeach2  BP.expression

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
  return $ ProcDecl name args Void bod
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
