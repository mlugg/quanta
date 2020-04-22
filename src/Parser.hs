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
sc  = L.space (void $ some $ oneOf " \t")   lineComment blockComment
scn :: Parser ()
scn = L.space (void $ some $ oneOf " \t\n") lineComment blockComment

identStart :: Parser Char
identStart = letterChar <|> char '_'
identChar :: Parser Char
identChar = alphaNumChar <|> oneOf "_'"
opChar :: Parser Char
opChar = oneOf "!#$%&*+./<=>?@\\^|-~;:"

reservedNames = [ "let", "in", "case", "of" ]
reservedOps   = [ "=", "::", "->", "\\" ]

reserved :: Parser () -> String -> Parser String
reserved sc' x = string x <* notFollowedBy identChar <* sc'

reservedOp :: Parser () -> String -> Parser String
reservedOp sc' x = string x <* notFollowedBy opChar <* sc'

operator :: Parser () -> Parser String
operator sc' = do x <- some opChar
                  sc'
                  if x `elem` reservedOps
                  then fail ("reserved operator " ++ show x ++ " cannot be used here")
                  else return x

identifier :: Parser () -> Parser String
identifier sc' = do x <- identStart
                    xs <- many identChar
                    sc'
                    let s = x:xs
                    if s `elem` reservedNames
                    then fail ("keyword " ++ show s ++ " cannot be identifier")
                    else return s

natural :: Parser () -> Parser Integer
natural sc' = L.decimal <* sc'

fileParser :: Parser [TopLevel]
fileParser = many topLevelDef <* eof

topLevelDef :: Parser TopLevel
topLevelDef = L.nonIndented scn $ try assignment <|> typeSig

assignment :: Parser TopLevel
assignment = L.lineFold scn $ \sc' -> TLAssign <$> identifier sc' <*> (reservedOp sc' "=" *> expr sc')

typeSig :: Parser TopLevel
typeSig = L.lineFold scn $ \sc' -> TLTypeSig <$> identifier sc' <*> (reservedOp sc' "::" *> typeExpr sc')

expr :: Parser () -> Parser Expr
expr sc' = makeExprParser (term sc')
  [ [ InfixL (ExprApplication <$ sc') ] ]

typeExpr :: Parser () -> Parser Type
typeExpr sc' = makeExprParser (typeTerm sc')
  [ [ InfixL (TypeApplication <$ sc') ]
  , [ InfixR (TypeFunction <$ reservedOp sc' "->") ] ]
 
term :: Parser () -> Parser Expr
term sc' = parens (expr sc')
       <|> ExprNatLit <$> natural sc'
       <|> ExprIdent  <$> identifier sc'
       <|> lambda sc'
  where parens = between (symbol "(") (symbol ")")
        symbol = L.symbol sc'

lambda :: Parser () -> Parser Expr
lambda sc' = ExprLambda
         <$> (reservedOp sc' "\\" *> identifier sc')
         <*> (reservedOp sc' "->" *> expr sc')

typeTerm :: Parser () -> Parser Type
typeTerm sc' = parens (typeExpr sc')
           <|> TypeIdent <$> identifier sc'
  where parens = between (symbol "(") (symbol ")")
        symbol = L.symbol sc'
