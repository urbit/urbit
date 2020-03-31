module Urbit.Uruk.Bracket
  ( Exp(..)
  , Out
  , SK(..)
  , naiveBracket
  , johnTrompBracket
  )
where

import ClassyPrelude hiding (union, (\\))
import Bound
import Data.Void

import Data.Deriving        (deriveEq1, deriveShow1)
import Data.Functor.Classes (eq1, showsPrec1)


-- Types -----------------------------------------------------------------------

infixl 5 :@;

data Exp b a
  = Lam b (Scope b (Exp b) a)
  | Var a
  | Exp b a :@ Exp b a
 deriving (Functor, Foldable, Traversable)

data SK a = S | K | V a
 deriving (Functor, Foldable, Traversable)

type Out = Exp Void


-- Instances -------------------------------------------------------------------

deriveEq1 ''Exp
deriveEq1 ''SK

deriveShow1 ''Exp
deriveShow1 ''SK

instance Eq a => Eq (SK a) where (==) = eq1
instance (Eq a, Eq b) => Eq (Exp a b) where (==) = eq1

instance Show a => Show (SK a) where showsPrec = showsPrec1
instance (Show a, Show b) => Show (Exp a b) where showsPrec = showsPrec1

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
  S   >>= _ = S
  K   >>= _ = K


-- Patterns --------------------------------------------------------------------

pattern VS = Var S
pattern VK = Var K

strip :: Traversable f => f (Var b a) -> Maybe (f a)
strip = traverse $ \case
  B _ -> Nothing
  F x -> Just x

pattern Raw b <- (strip -> Just b)
  where Raw = error "hack"

pattern FS = Var (F S)
pattern FK = Var (F K)
pattern VB a = Var (B a)

isComb :: Exp b (Var v (SK a)) -> Maybe (Exp b (Var v (SK z)))
isComb = strip >=> \case
  Var (V _) -> Nothing
  Var S     -> Just (Var (F S))
  Var K     -> Just (Var (F K))
  Lam _ _   -> Nothing
  _ :@ _    -> Nothing

pattern CB b <- (isComb -> Just b)
  where CB = error "hack"


-- Naive Bracket Abstraction Approach ------------------------------------------

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
    (:@) x y -> go x :@ go y
    Lam  _ b -> sk $ fmap sequence $ go $ fromScope b

  sk :: Out (Var b (SK a)) -> Out (SK a)
  sk = \case
    Raw b     -> VK :@ b
    Var (B _) -> VS :@ VK :@ VK
    Var (F v) -> Var v
    x   :@ y  -> VS :@ sk x :@ sk y
    Lam b  _  -> absurd b


-- John Tromp's Bracket Abtraction Approach ------------------------------------

{-
  Bracket Abstraction -- john Tromp's translation from lambdas to
  combinators. It adds some optimizations:

  ```
  λx.SKb    -> SK
  λx.mx     -> m             --  m does not reference x
  λx.xmx    -> λx.SSKxm
  λx.m(nb)  -> λx.S(λ_.m)nb  --  m,n are in {S,K}
  λx.(mn)b  -> λ_.Sm(λx.b)n  --  m,n are in {S,K}
  λx.mb(nb) -> λX.Smnb       --  m,n are {S,K} and b contains no lambda terms
  ```
-}
johnTrompBracket :: (Eq b, Eq a) => Exp b a -> Out (SK a)
johnTrompBracket = go
 where
  go :: (Eq a, Eq b) => Exp b a -> Out (SK a)
  go = \case
    Var x    -> Var (V x)
    x   :@ y -> go x :@ go y
    Lam _  b -> sk $ fmap sequence $ go $ fromScope b

  lam :: (Eq a, Eq v, Eq w) => Out (Var v (SK a)) -> Out (Var w (SK a))
  lam = fF . sk

  fF :: Functor f => f a -> f (Var b a)
  fF = fmap F

  sk :: (Eq a, Eq b) => Out (Var b (SK a)) -> Out (SK a)
  sk = \case
    FS :@ FK :@ _                     -> VS :@ VK
    Raw b                             -> VK :@ b
    Var (B _)                         -> VS :@ VK :@ VK
    Raw b         :@ VB _             -> b
    VB v :@ Raw m :@ VB _             -> sk (FS :@ FS :@ FK :@ VB v :@ fF m)
    CB m          :@ (CB n :@ b)      -> sk (FS :@ lam m :@ n :@ b)
    (CB m :@ n)   :@ l                -> sk (FS :@ m :@ lam l :@ n)
    (CB m :@ b) :@ (CB n :@ p) | b==p -> sk (FS :@ m :@ n :@ b)
    Var (F v)                         -> Var v
    x   :@ y                          -> VS :@ sk x :@ sk y
    Lam b  _                          -> absurd b
