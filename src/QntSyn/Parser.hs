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

identifier :: Parser Char -> Parser String
identifier start = try $
  do x <- start
     xs <- many identChar
     sc
     let s = x:xs
     if s `elem` reservedNames
     then fail ("keyword " ++ show s ++ " cannot be identifier")
     else return s

identType :: Parser String
identType = identifier upperChar

identVal :: Parser String
identVal = identifier $ lowerChar <|> char '_'

identAny :: Parser String
identAny = identifier $ letterChar <|> char '_'

natural :: Parser Integer
natural = L.decimal <* sc

-- }}}

-- Top-level parsers {{{

quantaFile :: Parser [TopLevel]
quantaFile = sc *> topLevelDef `endBy` semi <* eof

topLevelDef :: Parser TopLevel
topLevelDef = try assignment <|> typeSig

assignment :: Parser TopLevel
assignment = TLAssign <$> identVal <*> (reservedOp "=" *> expr)

typeSig :: Parser TopLevel
typeSig = TLTypeSig <$> identVal <*> (reservedOp "::" *> typeExpr)

-- }}}

-- Expression parsers {{{

expr :: Parser Expr
expr = makeExprParser term
  [ [ InfixL (pure EApplication)] ]
 
term :: Parser Expr
term = parens expr
   <|> ENatLit <$> natural
   <|> EIdent  <$> identAny
   <|> lambda
   <|> caseExpr
   <|> letExpr

lambda :: Parser Expr
lambda = ELambda
     <$> (reservedOp "\\" *> identVal)
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
      where letVar = (,) <$> identVal <*> (reservedOp "=" *> expr)
-- }}}

-- Type parsers {{{

parseTypeOp op = typeOp op <$ reservedOp op

typeExpr :: Parser Type
typeExpr = makeExprParser typeTerm
  [ [ InfixL (pure TApplication) ]
  , [ InfixR (parseTypeOp "->") ] ]

typeTerm :: Parser Type
typeTerm = parens typeExpr
       <|> TConcrete <$> identType

-- }}}

-- Pattern parsers {{{

patTerm :: Parser Pattern
patTerm = parens pattern
      <|> PNatLit <$> natural
      <|> PIdent  <$> identVal
      <|> PConstr <$> identType <*> pure []

pattern :: Parser Pattern
pattern = PConstr <$> identType <*> many patTerm
      <|> patTerm

-- }}}
