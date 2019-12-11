module Deppy.Core where

import ClassyPrelude

import Bound
import Data.Deriving (deriveEq1, deriveOrd1, deriveRead1, deriveShow1)
import Data.Maybe (fromJust)
import Data.Set (isSubsetOf)
import qualified Data.Set as Set
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
  | Cas (Typ a) (Exp a) (Map Tag (Exp a))
  -- recursion
  | Rec (Abs a)
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
  Cas t x cs >>= f = Cas (t >>= f) (x >>= f) (cs <&> (>>= f))
  Rec a >>= f = Rec (bindAbs a f)

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

-- typing environment
type Env a = a -> Typ a

extend :: (b -> Typ a) -> Env a -> Env (Var b a)
extend handleNewBindings oldEnv = \case
  -- TODO can we use Scope to decrease the cost of this?
  B v -> F <$> handleNewBindings v
  F v -> F <$> oldEnv v

extend1 :: Typ a -> Env a -> Env (Var () a)
extend1 t = extend \() -> t

-- amber rule assumptions
type Asm a = Set (Typ a, Typ a)

extendAsm :: (Ord a, Ord b) => Asm a -> Asm (Var b a)
extendAsm = Set.map \(t, u) -> (F <$> t, F <$> u)

type Typing = Maybe

-- TODO maybe this should be Typing () for error reporting?
-- think about env vs instantiate for bindings; if instantiate
-- as below, should the types be different?
-- better organize
nest :: Ord a => Env a -> Typ a -> Typ a -> Typing ()
nest env = fmap void . go env mempty
  where
    go :: Ord a => Env a -> Asm a -> Typ a -> Typ a -> Typing (Asm a)
    go env asm0 t0 u0 =
      if t0 == u0
        then pure asm0
        else let asm = Set.insert (t0, u0) asm0 in
          case (t0, u0) of
            (Typ, Typ) -> pure asm
            -- FIXME yeah actually I think this is wrong
            -- we're comaring the type of a type variable with 
            -- (Var v, u) -> go env asm (env v) u
            -- (t, Var v) -> go env asm t (env v)
            -- following Cardelli 80something, we check the RHSs assuming
            -- the putatively *lesser* of the LHSs for both
            (Fun (Abs a b), Fun (Abs a' b')) -> do
              asm <- go env asm a' a
              go (extend1 a' env) (extendAsm asm) (fromScope b) (fromScope b')
            (Cel (Abs a b), Cel (Abs a' b')) -> do
              asm <- go env asm a a'
              go (extend1 a env) (extendAsm asm) (fromScope b) (fromScope b')
            (Wut ls, Wut ls') -> do
              guard (ls `isSubsetOf` ls')
              pure asm
            -- TODO put into Typing errors
            (Lam{}, _) -> error "nest: lambda"
            (_, Lam{}) -> error "nest: lambda"
            (Cns{}, _) -> error "nest: cons"
            (_, Cns{}) -> error "nest: cons"
            (Tag{}, _) -> error "nest: tag"
            (_, Tag{}) -> error "nest: tag"
            (t@App{}, u) -> go env asm (whnf t) u
            (t, u@App{}) -> go env asm t (whnf u)
            (t@Hed{}, u) -> go env asm (whnf t) u
            (t, u@Hed{}) -> go env asm t (whnf u)
            (t@Tal{}, u) -> go env asm (whnf t) u
            (t, u@Tal{}) -> go env asm t (whnf u)
            (t@Cas{}, u) -> go env asm (whnf t) u
            (t, u@Cas{}) -> go env asm t (whnf u)
            (t@(Rec (Abs _ b)), u) -> go env asm (instantiate1 t b) u
            (t, u@(Rec (Abs _ b))) -> go env asm t (instantiate1 u b)

            
{-
nest :: Ord a => Env a -> Asm a -> Typ a -> Typ a -> Bool
nest _ _ Typ Typ = True
nest _ _ (Var v) (Var v') = v == v'  -- TODO amber for Rec
nest env asm (Var v) u = nest env asm (env v) u
nest env asm t (Var v) = nest env asm t (env v)
-- following Cardelli 80something, we check the RHSs assuming
-- the putatively *lesser* of the LHSs for both
nest env asm (Fun (Abs a b)) (Fun (Abs a' b')) =
  nest env asm a' a && nest (extend1 a' env) (extendAsm asm) (fromScope b) (fromScope b')
nest env asm (Cel (Abs a b)) (Cel (Abs a' b')) =
  nest env asm a a' && nest (extend1 a env) (extendAsm asm) (fromScope b) (fromScope b')
nest env asm (Wut ls) (Wut ls') = ls `isSubsetOf` ls'
nest _ _ Lam{} _ = error "nest: lambda"
nest _ _ _ Lam{} = error "nest: lambda"
nest _ _ Cns{} _ = error "nest: cons"
nest _ _ _ Cns{} = error "nest: cons"
nest _ _ Tag{} _ = error "nest: tag"
nest _ _ _ Tag{} = error "nest: tag"
nest env asm t@App{} u = nest env asm (whnf t) u
nest env asm t u@App{} = nest env asm t (whnf u)
nest env asm t@Hed{} u = nest env asm (whnf t) u
nest env asm t u@Hed{} = nest env asm t (whnf u)
nest env asm t@Tal{} u = nest env asm (whnf t) u
nest env asm t u@Tal{} = nest env asm t (whnf u)
-- TODO meet and join bro
nest env asm (Cas t _ _) u = nest env asm t u
nest env asm t (Cas u _ _) = nest env asm t u
nest env asm (Rec (Abs t b)) (Rec (Abs t' b')) = undefined
nest _ _ Rec{} _ = undefined
nest _ _ _ Rec{} = undefined
nest _ _ _ _ = False
-}

check :: Ord a => Env a -> Exp a -> Typ a -> Typing ()
check env e t = do
  t' <- infer env e
  nest env t' t

infer :: forall a. Ord a => Env a -> Exp a -> Typing (Typ a)
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
  App x y -> do
    Fun (Abs t b) <- infer env x
    check env y t
    pure $ whnf (instantiate1 y b)
  Hed x -> do
    Cel (Abs t _) <- infer env x
    pure t
  Tal x -> do
    Cel (Abs _ u) <- infer env x
    pure $ instantiate1 (whnf $ Hed $ x) u
  Cas t x cs -> do
    Typ <- infer env t
    Wut ts <- infer env x
    -- pretty restrictive - do we want?
    guard (ts == keysSet cs)
    traverse_ (\e -> check env e t) cs
    pure t
  Rec (Abs t b) -> do
    Typ <- infer env t
    -- todo can F <$> be made faster?
    check (extend1 t env) (fromScope b) (F <$> t)
    pure t

whnf :: Eq a => Exp a -> Exp a
whnf = \case
  App (whnf -> Lam (Abs _ b)) x -> whnf $ instantiate1 x b
  Hed (whnf -> Cns x _) -> whnf x
  Tal (whnf -> Cns _ y) -> whnf y
  Cas _ (whnf -> Tag t) cs -> whnf $ fromJust $ lookup t cs
  e@(Rec (Abs _ b)) -> whnf $ instantiate1 e b
  e -> e
