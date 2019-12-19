module Deppy.Core where

import ClassyPrelude

import Bound
import Data.Deriving (deriveEq1, deriveOrd1, deriveRead1, deriveShow1)
import Data.Maybe (fromJust)
import Data.Set (isSubsetOf)
import qualified Data.Set as Set
import Numeric.Natural
import Text.Show.Pretty (ppShow)

type Typ = Exp

data Exp a
  = Var a
  -- types
  | Typ                            -- #
  | Fun (Abs a)                    -- #<v/t t>  #-  v/t  t
  | Cel (Abs a)                    -- #[v/t t]  #:  v/t  t
  | Wut (Set Tag)                  -- #foo, ?(#foo #bar)
  -- introduction forms
  | Lam (Abs a)                    -- <v/t e>  |=  v/t  e
  | Cns (Exp a) (Exp a)            -- [e f]    :-  e f
  | Tag Tag                        -- %foo
  -- elimination forms
  | App (Exp a) (Exp a)            -- (e f)
  | Hed (Exp a)                    -- e.-
  | Tal (Exp a)                    -- e.+
  | Cas (Exp a) (Map Tag (Exp a))  -- ?%  e  $foo  f  $bar  g  ==
  -- recursion, flow control
  | Let (Exp a) (Scope () Exp a)   -- =/
  | Rec (Abs a)                    -- ..  v/t  e
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
  Cas x cs >>= f = Cas (x >>= f) (cs <&> (>>= f))
  Let a b >>= f = Let (a >>= f) (b >>>= f)
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

rec :: Eq a => a -> Typ a -> Exp a -> Exp a
rec v t e = Rec (Abs t (abstract1 v e))

ledt :: Eq a => a -> Exp a -> Exp a -> Exp a
ledt v e e' = Let e (abstract1 v e')

wut = Wut . setFromList

cas e cs = Cas e (mapFromList cs)

infixl 9 @:
(@:) = App

-- | typing environment
type Env a = a -> Typ a

extend :: (b -> Typ a) -> Env a -> Env (Var b a)
extend handleNewBindings oldEnv = \case
  -- TODO can we use Scope to decrease the cost of this?
  B v -> F <$> handleNewBindings v
  F v -> F <$> oldEnv v

extend1 :: Typ a -> Env a -> Env (Var () a)
extend1 t = extend \() -> t

-- | amber rule assumptions
type Asm a = Set (Typ a, Typ a)

extendAsm :: (Ord a, Ord b) => Asm a -> Asm (Var b a)
extendAsm = Set.map \(t, u) -> (F <$> t, F <$> u)

-- | Remove types that mention variables that are no longer in scope
retractAsm :: (Ord a, Ord b) => Asm (Var b a) -> Asm a
retractAsm = foldMap wither
  where
    wither = \case
      (cleanTyp -> Just t, cleanTyp -> Just u) -> singleton (t, u)
      _ -> mempty
    cleanTyp = traverse \case
      F v -> pure v
      B _ -> Nothing

type Typing = Maybe

tracePpShowId a = trace (ppShow a) a

traceAwesome s a b = trace (s <>":\n" <> ppShow a <> "\n" <> s <> " val:\n" <> ppShow b) b

-- TODO
--   - better errors
--   - state monad for Asm (how to handle polymorphic recursion?)
nest :: (Show a, Ord a) => Env a -> Typ a -> Typ a -> Typing ()
nest env = fmap void . go env mempty
  where
    go :: (Show a, Ord a) => Env a -> Asm a -> Typ a -> Typ a -> Typing (Asm a)
    -- FIXME use a better more aggro normal form
    go env asm0 (whnf -> t0) (whnf -> u0) =
      if t0 == u0 || member (t0, u0) asm0
        then pure asm0
        else let asm = Set.insert (t0, u0) asm0 in
          --traceAwesome "nest" (t0, u0)
          case (t0, u0) of
            (Typ, Typ) -> pure asm
            -- FIXME yeah actually I think this is wrong
            -- we're comaring the type of a type variable with 
            -- (Var v, u) -> go env asm (env v) u
            -- (t, Var v) -> go env asm t (env v)
            -- following Cardelli 80something, we check the RHSs assuming
            -- the putatively *lesser* of the LHSs for both
            (Fun (Abs a b), Fun (Abs a' b')) -> do
              asm' <- go env asm a' a
              retractAsm <$>
                go (extend1 a' env) (extendAsm asm') (fromScope b) (fromScope b')
            (Cel (Abs a b), Cel (Abs a' b')) -> do
              asm' <- go env asm a a'
              retractAsm <$>
                go (extend1 a env) (extendAsm asm') (fromScope b) (fromScope b')
            (Wut ls, Wut ls') -> do
              guard (ls `isSubsetOf` ls')
              pure asm
            -- Special rule for the Cas eliminator to enable sums and products
            -- TODO nf
            (Cas e cs, Cas e' cs') | whnf e == whnf e' -> do
                Wut s <- infer env e
                -- TODO I should thread changing asm through the traversal
                -- but I can't be bothered right now. Perf regression.
                asm <$ traverse_ chk (setToList s)
              where 
                chk tag = case (lookup tag cs, lookup tag cs') of
                  (Just t, Just u) -> go env asm t u
                  _ -> error "the Spanish inquisition"
            -- the below two rules are optimizations of
            -- (Cas e cs, u) -> go env asm (Cas e cs) (Cas e (mk u))
            -- (t, Cas e cs) -> go env asm (Cas e (mk t)) (Cas e cs)
            -- where mk = <map representing (const u)>
            (Cas e cs, u) -> do
              Wut (setToList -> s) <- infer env e
              -- TODO thread asms
              -- all of the cases nest in u
              asm <$ traverse_
                (\tag -> go env asm (fromJust $ lookup tag cs) u)
                s
            (t, Cas e cs) -> do
              Wut (setToList -> s) <- infer env e
              -- TODO thread asms
              -- u nests in all of the cases
              asm <$ traverse_
                (\tag -> go env asm t (fromJust $ lookup tag cs))
                s
            (t@(Rec (Abs _ b)), u) -> go env asm (instantiate1 t b) u
            (t, u@(Rec (Abs _ b))) -> go env asm t (instantiate1 u b)
            _ -> Nothing

check :: (Show a, Ord a) => Env a -> Exp a -> Typ a -> Typing ()
check env e t = do
  t' <- infer env e
  nest env t' t

infer :: forall a. (Show a, Ord a) => Env a -> Exp a -> Typing (Typ a)
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
  -- //  [@ 1]  #[# @]  ?<= #[t/# t]
  Cns x y -> do
    -- Infer non-dependent pairs; if you want dependency, you must annotate
    -- FIXME problem: [@ 1] not of type #[t/# t]; no way to create vases
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
  Cas x cs -> do
    Wut s <- infer env x
    -- TODO pretty restrictive - do we want?
    guard (s == keysSet cs)
    Cas x <$> traverse (infer env) cs
  -- Let e b -> do
  -- -- TODO is below faster, or infer env (instantiate1 e b)?
  -- t <- infer env e
  -- instantiate1 e $ infer (extend1 t env) (fromScope b)
  Rec (Abs t b) -> do
    Typ <- infer env t
    -- todo can F <$> be made faster?
    check (extend1 t env) (fromScope b) (F <$> t)
    pure t

whnf :: (Show a, Eq a) => Exp a -> Exp a
whnf = \case
  App (whnf -> Lam (Abs _ b)) x -> whnf $ instantiate1 x b
  Hed (whnf -> Cns x _) -> whnf x
  Tal (whnf -> Cns _ y) -> whnf y
  Cas (whnf -> Tag t) cs -> whnf $ fromJust $ lookup t cs
  e@(Rec (Abs _ b)) -> whnf $ instantiate1 e b
  e -> e
{-
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
-}

nf :: (Show a, Eq a) => Exp a -> Exp a
nf = traceShowId . \case
  Typ -> Typ
  _ -> undefined
