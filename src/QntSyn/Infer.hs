module QntSyn.Infer where

import Control.Monad.RWS.Lazy
import Control.Monad.Except
import qualified Data.Map as M
import Data.Maybe
import QntSyn

data TypeError = UnknownIdentError Identifier
  deriving (Show)

data Constraint = Equality Type Type
  deriving (Show)

type Infer a =
  RWST
    (M.Map Identifier Type)  -- Typing environment
    [Constraint]             -- Generated constraints
    Int                      -- Number of type vars
    (Except TypeError)       -- The exception monad for error handling
    a

typeVars = [0..] >>= \x -> (: show x) <$> ['a'..'z']

fresh :: Infer Type
fresh = get >>= \c -> TypeIdent (typeVars !! c) <$ put (c+1)

envLookup :: String -> TypeError -> Infer Type
envLookup x e = ask >>= maybe (throwError e) pure . M.lookup x

unify :: Type -> Type -> Infer ()
unify t1 t2 = tell [Equality t1 t2]

infer :: Expr -> Infer Type
infer e =
  case e of
    ExprIdent x -> envLookup x $ UnknownIdentError x

    ExprApplication f x ->
      infer f >>= \tf ->
      infer x >>= \tx ->
      fresh >>= \tr ->
      unify tf (typeOp "->" tx tr) >>
      return tr

    ExprNatLit x -> pure $ TypeIdent "Nat"

    ExprLambda x y ->
      fresh >>= \tx ->
        local
          (M.insert x tx)
          (typeOp "->" tx <$> infer y)

    ExprLet xs y -> undefined

    ExprCase x ys -> undefined
