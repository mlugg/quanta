module Parser where

import Text.Parsec
import Text.Parsec.Indent
import Text.Parsec.Expr
import qualified Text.Parsec.Token as T

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

langDef =
  T.LanguageDef { T.commentStart    = "{-"
                , T.commentEnd      = "-}"
                , T.commentLine     = "--"
                , T.nestedComments  = True
                , T.identStart      = letter <|> char '_'
                , T.identLetter     = alphaNum <|> oneOf "_'"
                , T.opStart         = T.opLetter langDef
                , T.opLetter        = oneOf "!#$%&*+./<=>?@\\^|-~;:"
                , T.reservedNames   = [ "let", "in", "case", "of" ]
                , T.reservedOpNames = [ "=", "::", "->", "\\" ]
                , T.caseSensitive   = True
                }

lexer = T.makeTokenParser langDef

identifier = T.identifier lexer
reserved   = T.reserved   lexer
operator   = T.operator   lexer
reservedOp = T.reservedOp lexer
natural    = T.natural    lexer
whiteSpace = T.whiteSpace lexer
parens     = T.parens     lexer

type EMCParser a = IndentParser String () a

parseFile :: String -> IO (Either ParseError [TopLevel])
parseFile f = doParse <$> readFile f

doParse :: String -> Either ParseError [TopLevel]
doParse = runIndentParser fileParser () "<no name>"

fileParser :: EMCParser [TopLevel]
fileParser = whiteSpace *> many (topLevel *> topLevelDef) <* eof

topLevelDef :: EMCParser TopLevel
topLevelDef = try assignment <|> try typeSig

assignment :: EMCParser TopLevel
assignment = withPos $
  (TLAssign <$> identifier) <+/> (reservedOp "=" *> expr)

typeSig :: EMCParser TopLevel
typeSig = withPos $
  (TLTypeSig <$> identifier) <+/> (reservedOp "::" *> typeExpr)

expr :: EMCParser Expr
expr = buildExpressionParser opTable term

opTable = [ [ Infix (pure ExprApplication) AssocLeft ] ]

term :: EMCParser Expr
term = sameOrIndented *>
  (
        parens expr
    <|> ExprNatLit <$> natural
    <|> ExprIdent  <$> identifier
    <|> lambda
  )

lambda :: EMCParser Expr
lambda = ExprLambda
     <$> (reservedOp "\\" *> identifier)
     <*> (reservedOp "->" *> expr)

typeExpr :: EMCParser Type
typeExpr = buildExpressionParser typeOpTable typeTerm

typeOpTable =
  [ [ Infix (pure TypeApplication) AssocLeft ]
  , [ Infix (TypeFunction <$ reservedOp "->") AssocRight ] ]

typeTerm :: EMCParser Type
typeTerm = sameOrIndented *>
  (
        parens typeExpr
    <|> TypeIdent <$> identifier
  )
