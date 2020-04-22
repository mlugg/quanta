module Parser where

import Data.Functor
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr
import Data.Void

type Identifier = String

data TopLevel = TLAssign Identifier Expr
              | TLTypeSig Identifier Type
              deriving (Show)

data Expr = ExprIdent Identifier
          | ExprApplication Expr Expr
          | ExprNatLit Integer
          | ExprLambda Identifier Expr
          deriving (Show)
          -- TODO: case / let

data Type = TypeIdent Identifier
          | TypeApplication Type Type
          | TypeFunction Type Type
          deriving (Show)

--langDef =
--  T.LanguageDef { T.commentStart    = "{-"
--                , T.commentEnd      = "-}"
--                , T.commentLine     = "--"
--                , T.nestedComments  = True
--                , T.identStart      = letter <|> char '_'
--                , T.identLetter     = alphaNum <|> oneOf "_'"
--                , T.opStart         = T.opLetter langDef
--                , T.opLetter        = oneOf "!#$%&*+./<=>?@\\^|-~;:"
--                , T.reservedNames   = [ "let", "in", "case", "of" ]
--                , T.reservedOpNames = [ "=", "::", "->", "\\" ]
--                , T.caseSensitive   = True
--                }

type Parser = Parsec Void String

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

operator :: Parser String
operator = do x <- some opChar
              sc
              if x `elem` reservedOps
              then fail ("reserved operator " ++ show x ++ " cannot be used here")
              else return x

identifier :: Parser String
identifier = do x <- identStart
                xs <- many identChar
                sc
                let s = x:xs
                if s `elem` reservedNames
                then fail ("keyword " ++ show s ++ " cannot be identifier")
                else return s

natural :: Parser Integer
natural = L.decimal <* sc

fileParser :: Parser [TopLevel]
fileParser = sc *> topLevelDef `endBy` semi <* eof

topLevelDef :: Parser TopLevel
topLevelDef = try assignment <|> typeSig

assignment :: Parser TopLevel
assignment = TLAssign <$> identifier <*> (reservedOp "=" *> expr)

typeSig :: Parser TopLevel
typeSig = TLTypeSig <$> identifier <*> (reservedOp "::" *> typeExpr)

expr :: Parser Expr
expr = makeExprParser term
  [ [ InfixL (pure ExprApplication)] ]

typeExpr :: Parser Type
typeExpr = makeExprParser typeTerm
  [ [ InfixL (pure TypeApplication) ]
  , [ InfixR (TypeFunction <$ reservedOp "->") ] ]
 
term :: Parser Expr
term = parens expr
   <|> ExprNatLit <$> natural
   <|> ExprIdent  <$> identifier
   <|> lambda

lambda :: Parser Expr
lambda = ExprLambda
     <$> (reservedOp "\\" *> identifier)
     <*> (reservedOp "->" *> expr)

typeTerm :: Parser Type
typeTerm = parens (typeExpr)
       <|> TypeIdent <$> identifier
