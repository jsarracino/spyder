module Language.Spyder.Parser.Lexer (
    spyderLexer
  , Parser
) where

import Text.Parsec
import Text.Parsec.Expr
import qualified Text.Parsec.Token as Tok

type Parser a = Parsec String () a

identStart :: Parser Char
identStart = letter <|> char '_' <|> char '$' <|> char '?'

identLetter :: Parser Char
identLetter = alphaNum <|> identStart

opStart :: Parser Char
opStart = oneOf "+*/-<>=!&|\\^@"

opLetter :: Parser Char
opLetter = oneOf "+*/-<>=!&|\\^@"

reservedNames :: [String]
reservedNames =
  [ "let", "in", "function", "for", "main", "procedure"] ++
  [ "Component", "foreach", "foreach2", "relation", "using", "always", "forall"]


reservedOpNames :: [String]
reservedOpNames =
  [ "*", "+", "/", "-"] ++
  ["&&", "||", "<", "<=", ">", ">="] ++
  ["==>", "<=>", "@", "^", "==", "!="]


spydLanDef :: Tok.LanguageDef ()
spydLanDef = Tok.LanguageDef {
  Tok.commentStart = "/*",
  Tok.commentEnd = "*/",
  Tok.commentLine = "//",
  Tok.nestedComments = True,
  Tok.caseSensitive = True,
  Tok.identStart = identStart,
  Tok.identLetter = identLetter,
  Tok.opStart = opStart,
  Tok.opLetter = opLetter,
  Tok.reservedNames = reservedNames,
  Tok.reservedOpNames = reservedOpNames
}

spyderLexer = Tok.makeTokenParser spydLanDef
