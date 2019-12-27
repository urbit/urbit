module Deppy.Core where

import ClassyPrelude

import Bound
import Bound.Name
import Bound.Scope
import Control.Arrow ((>>>))
import Control.Monad.Fail
import Control.Monad.Trans.Error
import Data.Deriving (deriveEq1, deriveOrd1, deriveRead1, deriveShow1)
import Data.Maybe (fromJust)
import Data.Set (isSubsetOf)
import Numeric.Natural
import Text.Show.Pretty (ppShow)
import qualified Data.Set as Set

type Typ = Exp

data Exp a
  = Var a
  -- types
  | Typ                                       -- #
  | Fun (Abs a)                               -- #<v/t t>  #-  v/t  t
  | Cel (Abs a)                               -- #[v/t t]  #:  v/t  t
  | Wut (Set Tag)                             -- #foo, ?(#foo #bar)
  -- introduction forms
  | Lam (Abs a)                               -- <v/t e>  |=  v/t  e
  | Cns (Exp a) (Exp a) (Maybe (Exp a))       -- [e f]    :-  e f
  | Tag Tag                                   -- %foo
  -- elimination forms
  | App (Exp a) (Exp a)                       -- (e f)
  | Hed (Exp a)                               -- e.-
  | Tal (Exp a)                               -- e.+
  | Cas (Exp a) (Map Tag (Exp a))             -- ?%  e  $foo  f  $bar  g  ==
  -- recursion, flow control
  | Let (Exp a) (Scope (Name Text ()) Exp a)  -- =/  v  e
  | Rec (Abs a)                               -- ..  v/t  e
  deriving (Functor, Foldable, Traversable)

type Tag = Natural

data Abs a = Abs
  { spec :: Typ a
  , body :: Scope (Name Text ()) Exp a
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

deriving instance Eq a   => Eq   (Abs a)
deriving instance Ord a  => Ord  (Abs a)
deriving instance Read a => Read (Abs a)
deriving instance Show a => Show (Abs a)

deriving instance Eq a   => Eq   (Exp a)
deriving instance Ord a  => Ord  (Exp a)
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
  Cns x y m >>= f = Cns (x >>= f) (y >>= f) ((>>= f) <$> m)
  Tag l >>= _ = Tag l
  App x y >>= f = App (x >>= f) (y >>= f)
  Hed x >>= f = Hed (x >>= f)
  Tal x >>= f = Tal (x >>= f)
  Cas x cs >>= f = Cas (x >>= f) (cs <&> (>>= f))
  Let a b >>= f = Let (a >>= f) (b >>>= f)
  Rec a >>= f = Rec (bindAbs a f)

bindAbs :: Abs a -> (a -> Exp b) -> Abs b
bindAbs (Abs s b) f = Abs (s >>= f) (b >>>= f)

lam :: Text -> Typ Text -> Exp Text -> Exp Text
lam v t e = Lam (Abs t (abstract1Name v e))

fun :: Text -> Typ Text -> Exp Text -> Exp Text
fun v t u = Fun (Abs t (abstract1Name v u))

fun_ :: Typ Text -> Typ Text -> Typ Text
fun_ t u = Fun (Abs t (abstractName (const Nothing) u))

cel :: Text -> Typ Text -> Exp Text -> Exp Text
cel v t u = Cel (Abs t (abstract1Name v u))

cel_ :: Typ Text -> Typ Text -> Typ Text
cel_ t u = Cel (Abs t (abstractName (const Nothing) u))

rec :: Text -> Typ Text -> Exp Text -> Exp Text
rec v t e = Rec (Abs t (abstract1Name v e))

ledt :: Text -> Exp Text -> Exp Text -> Exp Text
ledt v e e' = Let e (abstract1Name v e')

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

data TypeError where
  NestFail :: forall a. (Show a) => Typ a -> Typ a -> TypeError
  NotTyp   :: forall a. (Show a) => Typ a -> TypeError
  NotFun   :: forall a. (Show a) => Typ a -> TypeError
  NotCel   :: forall a. (Show a) => Typ a -> TypeError
  NotWut   :: forall a. (Show a) => Typ a -> TypeError
  Other    :: String -> TypeError

deriving instance Show TypeError

instance Eq TypeError where
  NestFail t u == NestFail t' u' = True  -- FIXME bleh
  Other s == Other s' = s == s'
  _ == _ = False

type Typing = Either TypeError

-- So that we can have refutable pattern binds in the typing monad.
-- TODO better error messages for these, also shouldn't they always
-- be expectFoos?
instance MonadFail Typing where
  fail = Left . Other

-- Needed for use of `guard` in the typing monad, even though it's deprecated.
-- WTF??
-- TODO replace guards with things that will generate better error msgs.
instance Error TypeError where
  strMsg = Other

-- Type Extractors -------------------------------------------------------------

free :: Applicative f => f a -> Scope b f a
free = Scope . pure . F

-- PROP equiv to nest _ Typ
expectTyp :: (Eq a, Show a) => Typ a -> Typing ()
expectTyp = whnf >>> \case
  Typ -> pure ()
  Cas _ cs -> traverse_ expectTyp cs
  e -> Left (NotTyp e)

expectFun :: (Eq a, Show a) => Typ a -> Typing (Abs a)
expectFun = whnf >>> \case
  Fun a -> pure a
  Cas e cs -> do
    absMap <- traverse expectFun cs
    let specMap = spec <$> absMap
    let bodyMap = body <$> absMap
    let body = Scope $ Cas (Var $ F e) (unscope <$> bodyMap)
    pure (Abs (Cas e specMap) body)
  e -> Left (NotFun e)

expectCel :: (Eq a, Show a) => Typ a -> Typing (Abs a)
expectCel = whnf >>> \case
  Cel a -> pure a
  Cas e cs -> do
    absMap <- traverse expectCel cs
    let specMap = spec <$> absMap
    let bodyMap = body <$> absMap
    let body = Scope $ Cas (Var $ F e) (unscope <$> bodyMap)
    pure (Abs (Cas e specMap) body)
  e -> Left (NotCel e)

expectWut :: (Eq a, Show a) => Typ a -> Typing (Set Tag)
expectWut = whnf >>> \case
  Wut s -> pure s
  -- the union of all the wuts
  Cas _ cs -> fold <$> traverse expectWut cs
  e -> Left (NotWut e)

-- Subtyping Check -------------------------------------------------------------

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

tracePpShowId a = trace (ppShow a) a

traceAwesome s a b = trace (s <>":\n" <> ppShow a <> "\n" <> s <> " val:\n" <> ppShow b) b

fromNScope :: Monad f => Scope (Name n b) f a -> f (Var b a)
fromNScope = fromScope . mapBound (\(Name _ b) -> b)

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
            -- following Cardelli 80something, we check the RHSs assuming
            -- the putatively *lesser* of the LHSs for both
            (Fun (Abs a b), Fun (Abs a' b')) -> do
              asm' <- go env asm a' a
              retractAsm <$>
                go (extend1 a' env) (extendAsm asm') (fromNScope b) (fromNScope b')
            (Cel (Abs a b), Cel (Abs a' b')) -> do
              asm' <- go env asm a a'
              retractAsm <$>
                go (extend1 a env) (extendAsm asm') (fromNScope b) (fromNScope b')
            (Wut ls, Wut ls') -> do
              -- guard (ls `isSubsetOf` ls')
              when (not (ls `isSubsetOf` ls')) $
                Left (NestFail t0 u0)
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
            _ -> Left (NestFail t0 u0)

-- Type Checking and Inference -------------------------------------------------

check :: (Show a, Ord a) => Env a -> Exp a -> Typ a -> Typing ()
check env e t = do
  t' <- infer env e
  nest env t' t

infer :: forall a. (Show a, Ord a) => Env a -> Exp a -> Typing (Typ a)
infer env = \case
  Var v -> pure $ env v
  Typ -> pure Typ
  Fun (Abs t b) -> do
    expectTyp =<< infer env t
    expectTyp =<< infer (extend1 t env) (fromNScope b) -- TODO more efficient?
    pure Typ
  Cel (Abs t b) -> do
    expectTyp =<< infer env t
    expectTyp =<< infer (extend1 t env) (fromNScope b)
    pure Typ
  Wut _ -> pure Typ
  Lam (Abs t b) -> do
    expectTyp =<< infer env t
    -- TODO hoist?
    t' <- toScope <$> infer (extend (const t) env) (fromScope b)
    pure $ Fun (Abs t t')
  Cns x y (Just t) -> do
    (Abs l r) <- expectCel t
    check env x l
    check env y (instantiate1 x r)
    pure t
  Cns x y Nothing -> do
    -- Infer non-dependent pairs; if you want dependency, you must annotate
    l <- infer env x
    r <- infer env y
    pure $ Cel (Abs l (free r))
  Tag t -> pure $ Wut (singleton t)
  App x y -> do
    (Abs t b) <- expectFun =<< infer env x
    check env y t
    pure $ whnf (instantiate1 y b)
  Hed x -> do
    (Abs t _) <- expectCel =<< infer env x
    pure t
  Tal x -> do
    (Abs _ u) <- expectCel =<< infer env x
    pure $ instantiate1 (whnf $ Hed $ x) u
  Cas x cs -> do
    s <- expectWut =<< infer env x
    guard (s `isSubsetOf` keysSet cs)
    Cas x <$> traverse (infer env) cs
  Let e b -> do
    _ <- infer env e  -- RHS must typecheck even if unused lol
    infer env (instantiate1 e b)  -- TODO efficiency?
  Rec (Abs t b) -> do
    expectTyp =<< infer env t
    -- todo can F <$> be made faster?
    check (extend1 t env) (fromNScope b) (F <$> t)
    pure t

-- Normal Forms ----------------------------------------------------------------

whnf :: (Show a, Eq a) => Exp a -> Exp a
whnf = \case
  App (whnf -> Lam (Abs _ b)) x -> whnf $ instantiate1 x b
  Hed (whnf -> Cns x _ _) -> whnf x
  Tal (whnf -> Cns _ y _) -> whnf y
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

nf :: (Eq a) => Exp a -> Exp a
nf = \case
  Typ -> Typ
  _ -> undefined
