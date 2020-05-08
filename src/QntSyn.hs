module QntSyn where

import qualified Data.Set as S

-- AST types {{{

type Identifier = String

data Pattern = PatIdent Identifier
             | PatApplication Pattern Pattern
             deriving (Show)

data TopLevel = TLAssign Identifier Expr
              | TLTypeSig Identifier Type
              deriving (Show)

data Expr = ExprIdent Identifier
          | ExprApplication Expr Expr
          | ExprNatLit Integer
          | ExprLambda Identifier Expr
          | ExprLet [(Identifier, Expr)] Expr
          | ExprCase Expr [(Pattern, Expr)]
          deriving (Show)

data Type = TypeVariable Integer
          | TypeConcrete Identifier
          | TypeApplication Type Type
          deriving (Show)

data TyScheme = TyScheme (S.Set Integer) Type
              deriving (Show)

-- }}}

typeOp op = TypeApplication . TypeApplication (TypeConcrete op)
