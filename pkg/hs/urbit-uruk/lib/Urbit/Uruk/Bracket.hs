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


-- Naive Bracket Abstraction Approach ------------------------------------------

{-
  Bracket Abstraction -- naive translation from lambdas to combinators.

  - `go` is the primary loop. It takes an lambda expression and
    translates it to combinators.

  - `goLam` takes a lambda expression whose body contains no lambda
    expressions and translates it to combinators.

  - `strip` removes the most recent binding if it is not used.
-}
naiveBracket :: Inp a -> Out (SK a)
naiveBracket = go
 where
  go :: Inp a -> Out (SK a)
  go = \case
    Var x    -> Var (V x)
    x   :@ y -> go x :@ go y
    Lam n  b -> fmap join $ goLam $ toScope $ fmap sequence $ go $ fromScope b

  goLam :: Scope () Out a -> Out (SK a)
  goLam = fromScope >>> \case
    (strip -> Just b) -> Var K :@ (V <$> b)
    Var (B ())        -> Var S :@ Var K :@ Var K
    Var (F x )        -> Var (V x)
    x   :@ y          -> Var S :@ goLam (toScope x) :@ goLam (toScope y)
    Lam b  s          -> absurd b

  strip :: Exp b (Var () a) -> Maybe (Exp b a)
  strip = traverse $ \case
    B () -> Nothing
    F x  -> Just x


-- John Tromp's Bracket Abtraction Approach ------------------------------------

{-
    Bracket Abstraction -- john Tromp's translation from lambdas to
    combinators.

    - `go` is the primary loop. It takes an lambda expression and
      translates it to combinators.

    - `goLam` takes a lambda expression whose body contains no lambda
      expressions and translates it to combinators.

    - `strip` removes the most recent binding if it is not used.
-}
trompBracket :: Inp a -> Out (SK a)
trompBracket = error "TODO"

{-
babs env (Lam x e)
  | Var "s" :@ Var"k" :@ _ <- t = Var "s" :@ Var "k"
  | x `notElem` fv [] t = Var "k" :@ t
  | Var y <- t, x == y  = Var "s" :@  Var "k" :@ Var "k"
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

isComb e = null $ fv [] e \\ ["s", "k"]

noLamEq (Var x) (Var y) = x == y
noLamEq (a :@ b) (c :@ d) = a `noLamEq` c && b `noLamEq` d
noLamEq _ _ = False
-}
