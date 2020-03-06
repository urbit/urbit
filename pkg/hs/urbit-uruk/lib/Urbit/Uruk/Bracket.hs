module Urbit.Uruk.Bracket where

import ClassyPrelude hiding (union, (\\))
import Bound
import Data.Void

import Control.Arrow ((>>>))
import Data.Function ((&))
import Data.List     (union, (\\))

-- Types -----------------------------------------------------------------------

infixl 5 :@;

data Exp b a
  = Lam b (Scope b (Exp b) a)
  | Var a
  | Exp b a :@ Exp b a
 deriving (Functor, Foldable, Traversable)

data SK a
  = S
  | K
  | V a
 deriving (Functor, Foldable, Traversable)

type Inp = Exp ()
type Out = Exp Void


-- Instances -------------------------------------------------------------------

instance Applicative (Exp b) where
  pure  = Var
  (<*>) = ap

instance Monad (Exp b) where
  return = Var
  Var a    >>= f = f a
  Lam  v b >>= f = Lam v (b >>>= f)
  (:@) x y >>= f = (:@) (x >>= f) (y >>= f)

instance Applicative SK where
  pure  = V
  (<*>) = ap

instance Monad SK where
  return = V
  V a >>= f = f a
  S   >>= f = S
  K   >>= f = S

pattern VS = Var S
pattern VK = Var K


-- Naive Bracket Abstraction Approach ------------------------------------------

throughScope
  :: forall m n v w a b
   . (Monad m, Monad n)
  => (n (Var v a) -> m (Var w b))
  -> Scope v n a
  -> Scope w m b
throughScope f = toScope . f . fromScope

strip :: Traversable f => f (Var b a) -> Maybe (f a)
strip = traverse $ \case
  B _ -> Nothing
  F x -> Just x

{-
  Bracket Abstraction -- naive translation from lambdas to combinators.

  - `go` is the primary loop. It takes an lambda expression and
    translates it to combinators.

  - `goLam` takes a lambda expression whose body contains no lambda
    expressions and translates it to combinators.
-}
naiveBracket :: Exp b a -> Out (SK a)
naiveBracket = go
 where
  go :: Exp b a -> Out (SK a)
  go = \case
    Var x    -> Var (V x)
    x   :@ y -> go x :@ go y
    Lam n  b -> toSK $ throughScope (fmap sequence . go) b

  toSK :: Scope b Out (SK a) -> Out (SK a)
  toSK = fromScope >>> \case
    (strip -> Just b) -> VK :@ b
    Var (B _)         -> VS :@ VK :@ VK
    Var (F v)         -> Var v
    x :@ y            -> VS :@ toSK (toScope x) :@ toSK (toScope y)
    Lam b  s          -> absurd b

-- John Tromp's Bracket Abtraction Approach ------------------------------------

pattern VFS = Var (F S)
pattern VFK = Var (F K)

{-
  Bracket Abstraction -- john Tromp's translation from lambdas to
  combinators. It adds some optimizations:

  ```
  λx.SK_    -> SK
  λx.mx     -> m             --  m does not reference x
  λx.xmx    -> λx.SSKxm
  λx.m(nb)  -> λx.S(λ_.m)nb  --  m,n are in {S,K}
  λx.(mn)b  -> λ_.Sm(λx.b)n  --  m,n are in {S,K}
  λx.mb(nb) -> λX.Smnb       --  m,n are {S,K} and b contains no lambda terms
  ```

  This changes the shape of the computation somewhat, since simplification
  rules can re-introduce lambda expressions.
-}
trompBracket :: Exp b a -> Out (SK a)
trompBracket = go
 where
  go :: Exp b a -> Out (SK a)
  go = \case
    Var x    -> Var (V x)
    x   :@ y -> go x :@ go y
    Lam n  b -> toSK $ throughScope (fmap sequence . go) b

  toSK :: Scope b Out (SK a) -> Out (SK a)
  toSK = fromScope >>> \case
    VFS :@ VFK :@ _   -> VS :@ VK
    (strip -> Just b) -> VK :@ b
    Var (B _)         -> VS :@ VK :@ VK
    Var (F v)         -> Var v
    x :@ y            -> VS :@ toSK (toScope x) :@ toSK (toScope y)
    Lam b  s          -> absurd b

  --  Maybe not necessary
  inp :: Out a -> Exp b a
  inp = \case
    Var x    -> Var x
    (:@) x y -> inp x :@ inp y
    Lam v _  -> absurd v

{-
  | m :@ Var y <- t, x == y, x `notElem` fv [] m = m
  | Var y :@ m :@ Var z <- t, x == y, x == z =
    babs env $ Lam x $ Var "s" :@ Var "s" :@ Var "k" :@ Var x :@ m
  | m :@ (n :@ l) <- t, isComb m, isComb n =
    babs env $ Lam x $ Var "s" :@ Lam x m :@ n :@ l
  | (m :@ n) :@ l <- t, isComb m, isComb l =
    babs env $ Lam x $ Var "s" :@ m :@ Lam x l :@ n
  | (m :@ l) :@ (n :@ l') <- t, l `noLamEq` l', isComb m, isComb n
    = babs env $ Lam x $ Var "s" :@ m :@ n :@ l
  | m :@ n <- t        = Var "s" :@ babs env (Lam x m) :@ babs env (Lam x n)
  where t = babs env e
babs env (Var s)
  | Just t <- lookup s env = babs env t
  | otherwise              = Var s
babs env (m :@ n) = babs env m :@ babs env n

-}
