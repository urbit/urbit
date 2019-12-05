module Deppy.Core where

import ClassyPrelude

import Bound
import Data.Deriving (deriveEq1, deriveOrd1, deriveRead1, deriveShow1)
import Data.Set (isSubsetOf)
import Numeric.Natural

type Typ = Exp

data Exp a
  = Var a
  -- types
  | Typ
  | Fun (Abs a)
  | Cel (Abs a)
  | Wut (Set Tag)
  -- introduction forms
  | Lam (Abs a)
  | Cns (Exp a) (Exp a)
  | Tag Tag
  -- elimination forms
  | App (Exp a) (Exp a)
  | Hed (Exp a)
  | Tal (Exp a)
  deriving (Functor, Foldable, Traversable)

type Tag = Natural

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
  Typ   >>= _ = Typ
  Fun a >>= f = Fun (bindAbs a f)
  Cel a >>= f = Cel (bindAbs a f)
  Wut ls >>= _ = Wut ls
  Lam a >>= f = Lam (bindAbs a f)
  Cns x y >>= f = Cns (x >>= f) (y >>= f)
  Tag l >>= _ = Tag l
  App x y >>= f = App (x >>= f) (y >>= f)
  Hed x >>= f = Hed (x >>= f)
  Tal x >>= f = Tal (x >>= f)

bindAbs :: Abs a -> (a -> Exp b) -> Abs b
bindAbs (Abs s b) f = Abs (s >>= f) (b >>>= f)

lam :: Eq a => a -> Typ a -> Exp a -> Exp a
lam v t e = Lam (Abs t (abstract1 v e))

fun :: Eq a => a -> Typ a -> Typ a -> Typ a
fun v t u = Fun (Abs t (abstract1 v u))

fun_ :: Typ a -> Typ a -> Typ a
fun_ t u = Fun (Abs t (abstract (const Nothing) u))

cel :: Eq a => a -> Typ a -> Typ a -> Typ a
cel v t u = Cel (Abs t (abstract1 v u))

cel_ :: Typ a -> Typ a -> Typ a
cel_ t u = Cel (Abs t (abstract (const Nothing) u))

infixl 9 @:
(@:) = App

type Env a = a -> Typ a

extend :: (b -> Typ a) -> Env a -> Env (Var b a)
extend handleNewBindings oldEnv = \case
  -- TODO can we use Scope to decrease the cost of this?
  B v -> F <$> handleNewBindings v
  F v -> F <$> oldEnv v

extend1 :: Typ a -> Env a -> Env (Var () a)
extend1 t = extend \() -> t

type Typing = Maybe

-- TODO maybe this should be Typing () for error reporting?
-- think about env vs instantiate for bindings; if instantiate
-- as below, should the types be different?
-- better organize
nest :: Eq a => Env a -> Typ a -> Typ a -> Bool
nest _ Typ Typ = True
nest _ (Var v) (Var v') = v == v'  -- TODO amber for Rec
nest env (Var v) u = nest env (env v) u
nest env t (Var v) = nest env t (env v)
-- following Cardelli 80something, we check the RHSs assuming
-- the putatively *lesser* of the LHSs for both
nest env (Fun (Abs a b)) (Fun (Abs a' b')) =
  nest env a' a && nest (extend1 a' env) (fromScope b) (fromScope b')
nest env (Cel (Abs a b)) (Cel (Abs a' b')) =
  nest env a a' && nest (extend1 a env) (fromScope b) (fromScope b')
nest env (Wut ls) (Wut ls') = ls `isSubsetOf` ls'
nest _ Lam{} _ = error "nest: lambda"
nest _ _ Lam{} = error "nest: lambda"
nest _ Cns{} _ = error "nest: cons"
nest _ _ Cns{} = error "nest: cons"
nest _ Tag{} _ = error "nest: tag"
nest _ _ Tag{} = error "nest: tag"
nest env t@App{} u = nest env (whnf t) u
nest env t u@App{} = nest env t (whnf u)
nest env t@Hed{} u = nest env (whnf t) u
nest env t u@Hed{} = nest env t (whnf u)
nest env t@Tal{} u = nest env (whnf t) u
nest env t u@Tal{} = nest env t (whnf u)
nest _ _ _ = False

check :: Eq a => Env a -> Exp a -> Typ a -> Typing ()
check env e t = do
  t' <- infer env e
  guard (nest env t t')

infer :: forall a. Eq a => Env a -> Exp a -> Typing (Typ a)
infer env = \case
  Var v -> pure $ env v
  Typ -> pure Typ
  Fun (Abs t b) -> do
    Typ <- infer env t
    Typ <- infer (extend1 t env) (fromScope b)
    pure Typ
  Cel (Abs t b) -> do
    Typ <- infer env t
    Typ <- infer (extend1 t env) (fromScope b)
    pure Typ
  Wut _ -> pure Typ
  Lam (Abs t b) -> do
    -- TODO do I need (whnf -> Typ)? (and elsewhere)
    Typ <- infer env t
    (toScope -> t') <- infer (extend1 t env) (fromScope b)
    pure $ Fun (Abs t t')
  Cns x y -> do
    -- Infer non-dependent pairs; if you want dependency, you must annotate
    t <- infer env x
    u <- infer env y
    pure $ Cel (Abs t (abstract (const Nothing) u))
  Tag t -> pure $ Wut (singleton t)
  Hed x -> do
    Cel (Abs t _) <- infer env x
    pure t
  Tal x -> do
    Cel (Abs _ u) <- infer env x
    pure $ instantiate1 (whnf $ Hed $ x) u
  App x y -> do
    Fun (Abs t b) <- infer env x
    check env y t
    pure $ whnf (instantiate1 y b)

whnf :: Eq a => Exp a -> Exp a
whnf = \case
  App (whnf -> Lam (Abs _ b)) x -> instantiate1 x b
  Hed (whnf -> Cns x _) -> x
  Tal (whnf -> Cns _ y) -> y
  e -> e
