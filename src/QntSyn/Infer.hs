module QntSyn.Infer where

import Control.Monad.RWS.Lazy
import Control.Monad.Except
import qualified Data.Map as M
import Data.Maybe
import QntSyn

-- |The errors which can occur during type inference.
data TypeError = UnknownIdentError Identifier
  deriving (Show)

-- |The constraints which can be generated by the 'infer' function, to be
-- later solved.
data Constraint = Equality Type Type
  deriving (Show)

-- |The monad within which constraint generation runs. This is an RWS
-- (reader/writer/state) monad, with a read-only typing environment; a
-- writable list of constraints; and a integer state representing the
-- number of instantiated unification variables (to prevent conflicts).
-- It also contains an 'Except' monad in the stack for error reporting.
type Infer a =
  RWST
    (M.Map Identifier Type)  -- Typing environment
    [Constraint]             -- Generated constraints
    Int                      -- Number of type vars
    (Except TypeError)       -- The exception monad for error handling
    a

-- |Generates a new unification variable.
fresh :: Infer Type
fresh = get >>= \c -> TypeUnification c <$ put (c+1)

-- |Looks up the given identifier in the typing environment, throwing
-- the given 'TypeError' if it does not exist.
envLookup :: String -> TypeError -> Infer Type
envLookup x e = ask >>= maybe (throwError e) pure . M.lookup x

-- |The main type inferrence function. Given an expression and initial
-- environment, returns the type of the expression, as well as
-- (indirectly, via the 'Infer' monad) a list of constraints to solve.
infer :: Expr -> Infer Type
infer e =
  case e of
    ExprIdent x -> envLookup x $ UnknownIdentError x

    ExprApplication f x ->
      infer f >>= \tf ->
      infer x >>= \tx ->
      fresh >>= \tr ->
      tell [Equality tf (typeOp "->" tx tr)] >>
      return tr

    ExprNatLit x -> pure $ TypeConcrete "Nat"

    ExprLambda x y ->
      fresh >>= \tx ->
        local
          (M.insert x tx)
          (typeOp "->" tx <$> infer y)

    -- TODO
    ExprLet xs y -> undefined

    -- TODO
    ExprCase x ys -> undefined