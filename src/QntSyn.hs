module QntSyn where

import qualified Data.Set as S

-- AST types {{{

type Identifier = String

data Pattern = PConstr Identifier [Pattern]
             | PIdent Identifier
             | PNatLit Integer
             deriving (Show)

data TopLevel = TLAssign Identifier Expr
              | TLTypeSig Identifier Type
              deriving (Show)

data Expr = EIdent Identifier
          | EApplication Expr Expr
          | ENatLit Integer
          | ELambda Identifier Expr
          | ELet [(Identifier, Expr)] Expr
          | ECase Expr [(Pattern, Expr)]
          deriving (Show)

data Type = TVariable Integer
          | TConcrete Identifier
          | TApplication Type Type
          deriving (Show)

data TyScheme = TyScheme (S.Set Integer) Type
              deriving (Show)

-- }}}

typeOp op = TApplication . TApplication (TConcrete op)
