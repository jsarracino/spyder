module Language.Spyder.Parser.Parser (
    expr
  , stmt
  , prog
) where

import Text.Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Token as Tok
import Language.Spyder.Parser.Lexer         (spyderLexer, Parser)
import Language.Spyder.AST.Imp
import Language.Spyder.AST.Spec

import Control.Monad                        (liftM, liftM2)

lexer = spyderLexer
res = Tok.reserved lexer
parens = Tok.parens lexer
symb = Tok.symbol lexer
ident = Tok.identifier lexer
semi = Tok.semi lexer
commas = Tok.commaSep lexer
braces = Tok.braces lexer
brackets = Tok.brackets lexer

followedBy :: Parser a -> Parser b -> Parser a
followedBy p q = do {r <- p; q; return r}

spaced :: Parser a -> Parser [a]
spaced p = p `sepBy` (Tok.whiteSpace lexer)
semis p = p `sepBy` semi

ints = liftM IConst $ Tok.integer lexer
vars = liftM VConst ident
bools = tru <|> fls
  where
    tru = do {res "true"; return $ BConst True }
    fls = do {res "false"; return $ BConst False }

arrAccess = liftM2 Index vars (brackets expr)
arrPrim = liftM AConst (brackets $ commas expr)

 -- TODO:
 -- | UnOP Uop Expr
 -- | Index Expr Expr
term :: Parser Expr
term    =  parens expr
  <|> try ints
  <|> try bools
  <|> try arrPrim
  <|> try arrAccess
  <|> try vars
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

binary name fun assoc =
  Infix (do{ Tok.reservedOp lexer name; return fun }) assoc

loopP :: Parser Statement
loopP = do {
  res "for";
  vars <- parens $ commas $ ident;
  res "in";
  arrs <- parens $ commas $ expr;
  body <- braces stmt;
  return $ Loop vars arrs body
}

declP :: Parser Statement
declP = do {
  res "let";
  vname <- ident;
  rhs <- (symb "=") >> expr;
  semi;
  return $ Decl vname rhs
}

stmt :: Parser Statement
stmt = liftM Seq $ spaced stmt'

stmt' :: Parser Statement
stmt' =
      try declP
  <|> try loopP
  <|> try (liftM2 Assgn expr ((symb "=" >> expr) `followedBy` semi))
  <|> try (do {res "Ensure;"; return Ensure})
  <?> "Statement"

arrInv :: Parser InvDecl
arrInv = do {
  l <- expr;
  symb "~";
  r <- expr;
  symb "|";
  body <- expr;
  return $ ArrInv l r body
}

inv :: Parser InvDecl
inv =
      try arrInv
  <|> liftM BaseInv expr
  <?> "Invariant Decl"

prog :: Parser Program
prog = do {
  res "Require";
  invs <- braces $ semis inv;
  res "in";
  body <- braces stmt;
  return (invs, body);
}
