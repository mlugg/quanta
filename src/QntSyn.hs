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

data ADTDef = ADTDef [Integer] [( Identifier, [Type] )]
            deriving (Show)

-- ADTConstr typename typeargcount args
-- In the args, tyvars are the type constructor args, from 0 up
data ADTConstr = ADTConstr Identifier [Integer] [Type]

-- }}}

typeOp op = TApplication . TApplication (TConcrete op)
