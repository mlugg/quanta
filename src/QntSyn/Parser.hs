module QntSyn.Parser(quantaFile) where

import Data.Functor
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr
import Data.Void

import QntSyn

type Parser = Parsec Void String

-- Utility parsers {{{

lineComment :: Parser ()
lineComment  = L.skipLineComment  "--"
blockComment :: Parser ()
blockComment = L.skipBlockComment "{-" "-}"

sc :: Parser ()
sc = L.space (void $ some $ oneOf " \t\n") lineComment blockComment

identStart :: Parser Char
identStart = letterChar <|> char '_'
identChar :: Parser Char
identChar = alphaNumChar <|> oneOf "_'"
opChar :: Parser Char
opChar = oneOf "!#$%&*+./<=>?@\\^|-~:"

reservedNames = [ "let", "in", "case", "of" ]
reservedOps   = [ "=", "::", "->", "\\" ]

reserved :: String -> Parser String
reserved x = string x <* notFollowedBy identChar <* sc

reservedOp :: String -> Parser String
reservedOp x = string x <* notFollowedBy opChar <* sc

symbol :: Tokens String -> Parser (Tokens String)
symbol = L.symbol sc

semi :: Parser String
semi = symbol ";"

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

operator :: Parser String
operator = try $ do x <- some opChar
                    sc
                    if x `elem` reservedOps
                    then fail ("reserved operator " ++ show x ++ " cannot be used here")
                    else return x

identifier :: Parser String
identifier = try $ do x <- identStart
                      xs <- many identChar
                      sc
                      let s = x:xs
                      if s `elem` reservedNames
                      then fail ("keyword " ++ show s ++ " cannot be identifier")
                      else return s

natural :: Parser Integer
natural = L.decimal <* sc

-- }}}

-- Top-level parsers {{{

quantaFile :: Parser [TopLevel]
quantaFile = sc *> topLevelDef `endBy` semi <* eof

topLevelDef :: Parser TopLevel
topLevelDef = try assignment <|> typeSig

assignment :: Parser TopLevel
assignment = TLAssign <$> identifier <*> (reservedOp "=" *> expr)

typeSig :: Parser TopLevel
typeSig = TLTypeSig <$> identifier <*> (reservedOp "::" *> typeExpr)

-- }}}

-- Expression parsers {{{

expr :: Parser Expr
expr = makeExprParser term
  [ [ InfixL (pure EApplication)] ]
 
term :: Parser Expr
term = parens expr
   <|> ENatLit <$> natural
   <|> EIdent  <$> identifier
   <|> lambda
   <|> caseExpr
   <|> letExpr

lambda :: Parser Expr
lambda = ELambda
     <$> (reservedOp "\\" *> identifier)
     <*> (reservedOp "->" *> expr)

caseExpr :: Parser Expr
caseExpr = ECase
       <$> (reserved "case" *> expr)
       <*> (reserved "of"   *> braces (caseBranch `sepEndBy` semi))
       where caseBranch = (,) <$> pattern <*> (reservedOp "->" *> expr)

letExpr :: Parser Expr
letExpr = ELet
      <$> (reserved "let" *> braces (letVar `sepEndBy` semi))
      <*> (reserved "in"  *> expr)
      where letVar = (,) <$> identifier <*> (reservedOp "=" *> expr)
-- }}}

-- Type parsers {{{

parseTypeOp op = typeOp op <$ reservedOp op

typeExpr :: Parser Type
typeExpr = makeExprParser typeTerm
  [ [ InfixL (pure TApplication) ]
  , [ InfixR (parseTypeOp "->") ] ]

typeTerm :: Parser Type
typeTerm = parens typeExpr
       <|> TConcrete <$> identifier

-- }}}

-- Pattern parsers {{{

pattern :: Parser Pattern
pattern = makeExprParser patternTerm
  [ [ InfixL (pure PApplication) ] ]

patternTerm :: Parser Pattern
patternTerm = parens pattern
          <|> PNatLit <$> natural
          <|> PIdent  <$> identifier

-- }}}
