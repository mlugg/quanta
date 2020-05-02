module QntSyn where

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
          | ExprLet [LetBinding] Expr
          | ExprCase Expr [(Pattern, Expr)]
          deriving (Show)

data LetBinding = LetAssign Identifier Expr
                | LetTypeSig Identifier Type
                deriving (Show)

data Type = TypeUnification Int
          | TypeConcrete Identifier
          | TypeApplication Type Type
          deriving (Show)

-- }}}

typeOp op = TypeApplication . TypeApplication (TypeConcrete op)
