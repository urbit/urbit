module Deppy.Core where

import ClassyPrelude

import Bound
import Control.Category ((<<<), (>>>))
import Data.Deriving (deriveEq1, deriveOrd1, deriveRead1, deriveShow1)
import Data.Map (foldlWithKey)
import Numeric.Natural

type Typ a = Exp a

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

deriveEq1 ''Abs
deriveOrd1 ''Abs
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

subst :: (Eq a, Ord a) => Map a (Exp a) -> Exp a -> Exp a
subst s e = foldlWithKey step e s
  where
    step e v r = substitute v r e

extend :: (b -> Typ a) -> (a -> Typ a) -> (Var b a -> Typ a)
extend b env = \case
  B v -> b v
  F v -> env v

extend1 :: Typ a -> (a -> Typ a) -> (Var () a -> Typ a)
extend1 t = extend \() -> t

infer :: forall a. (a -> Typ a) -> Exp a -> Typ a
infer env = \case
  Var v -> env v
  Uni n -> Uni (n + 1)
  Fun (Abs t b) -> Uni (max n k)
    where
      n = extractUni $ infer env t
      k = extractUni $ infer (extend1 t env) (fromScope b)
  Lam (Abs t b) -> Fun (Abs t t')
    where
      -- FIXME require t to be in a universe
      t' :: Scope () Exp a
      t' = infer (extend1 t env :: Var () a -> Typ a) (fromScope b :: Var () a)
  App x y -> t'
    where
      (Abs t b) = extractFun $ infer env x
      t' = undefined

extractUni :: Exp a -> Natural
extractUni = normalize >>> \case
  Uni n -> n

extractFun :: Exp a -> Abs a
extractFun = normalize >>> \case
  Fun a -> a

normalize :: Exp a -> Exp a
normalize = undefined
