module Deppy.Core where

import ClassyPrelude

import Bound
import Control.Category ((<<<), (>>>))
import Data.Deriving (deriveEq1, deriveOrd1, deriveRead1, deriveShow1)
import Data.Map (foldlWithKey)
import Numeric.Natural

type Typ = Exp

data Exp a
  = Var a
  | Uni Natural
  | Fun (Abs a)
  | Lam (Abs a)
  | App (Exp a) (Exp a)
  deriving (Functor, Foldable, Traversable)

data Abs a = Abs
  { spec :: Typ a
  , body :: Scope () Exp a
  }
  deriving (Functor, Foldable, Traversable)

deriveEq1   ''Abs
deriveOrd1  ''Abs
deriveRead1 ''Abs
deriveShow1 ''Abs
--makeBound   ''Abs

deriveEq1   ''Exp
deriveOrd1  ''Exp
deriveRead1 ''Exp
deriveShow1 ''Exp
--makeBound   ''Exp

deriving instance Eq a   => Eq (Abs a)
deriving instance Ord a  => Ord (Abs a)
deriving instance Read a => Read (Abs a)
deriving instance Show a => Show (Abs a)

deriving instance Eq a   => Eq (Exp a)
deriving instance Ord a  => Ord (Exp a)
deriving instance Read a => Read (Exp a)
deriving instance Show a => Show (Exp a)

instance Applicative Exp where
  pure = Var
  (<*>) = ap

instance Monad Exp where
  return = Var
  Var a >>= f = f a
  Uni n >>= _ = Uni n
  Fun a >>= f = Fun (bindAbs a f)
  Lam a >>= f = Lam (bindAbs a f)
  App x y >>= f = App (x >>= f) (y >>= f)

bindAbs :: Abs a -> (a -> Exp b) -> Abs b
bindAbs (Abs s b) f = Abs (s >>= f) (b >>>= f)

lam :: Eq a => a -> Typ a -> Exp a -> Exp a
lam v t e = Lam (Abs t (abstract1 v e))

fun :: Eq a => a -> Typ a -> Exp a -> Typ a
fun v t e = Fun (Abs t (abstract1 v e))

type Env a = a -> Typ a

extend :: (b -> Typ a) -> Env a -> Env (Var b a)
extend handleNewBindings oldEnv = \case
  -- TODO can we use Scope to decrease the cost of this?
  B v -> F <$> handleNewBindings v
  F v -> F <$> oldEnv v

extend1 :: Typ a -> Env a -> Env (Var () a)
extend1 t = extend \() -> t

type Typing = Maybe

check :: Eq a => Env a -> Exp a -> Typ a -> Typing ()
check env e t = do
  t' <- infer env e
  guard (t == t')

infer :: forall a. Eq a => Env a -> Exp a -> Typing (Typ a)
infer env = \case
  Var v -> pure $ env v
  Uni n -> pure $ Uni (n + 1)
  Lam (Abs t b) -> do
    Uni _ <- infer env t
    (toScope -> t') <- infer (extend1 t env) (fromScope b)
    pure $ Fun (Abs t t')
  Fun (Abs t b) -> do
    Uni n <- infer env t
    Uni k <- infer (extend1 t env) (fromScope b)
    pure $ Uni (max n k)
  App x y -> do
    Fun (Abs t b) <- infer env x
    check env y t
    pure $ whnf (instantiate1 y b)

whnf :: Eq a => Exp a -> Exp a
whnf = \case
  App (whnf -> Lam (Abs _ b)) x -> instantiate1 x b
  e -> e
