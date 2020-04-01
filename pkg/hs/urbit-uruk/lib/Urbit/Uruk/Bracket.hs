{- |
  Compile lambda calculus to SK combinators using "bracket abstraction".
-}
module Urbit.Uruk.Bracket
  ( Exp(..)
  , Out
  , SK(..)
  , naiveBracket
  , johnTrompBracket
  , outToUruk
  )
where

import ClassyPrelude hiding (union, (\\))
import Bound
import Data.Void

import Data.Deriving        (deriveEq1, deriveShow1)
import Data.Functor.Classes (eq1, showsPrec1)
import Urbit.Uruk.Class     (Uruk(..))


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

{-
  An expression that contains no reference to enclosing lambda. Contains
  no variable reference with de-bruijn index 0.

  The result of the pattern matching, if successful, strips off one
  binding binding (decrements all de-bruijn indices).
-}
pattern Cns b <- (strip -> Just b)
  where Cns = error "hack"

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

    - Variables are unchanged.
    - Recurse through function application.
    - For lambdas
      - First recurse into body (transform the body using `go`).
        - This will produce an expression that contains no lambdas,
          but may still reference variables bound by this lambda, or by
          outer lambdas.
        - Then, use `abs` to get rid of the lambda binding itself.

  - `abs` takes a lambda body that has already been transformed
    (contains no lambda expressions), and converts a single binding into
    SK combinators:

    - Using these rules:

      ```
      [λx.x]  -> SKK
      [λx.b]  -> Kb  --  Where b contains no refernce to `x`.
      [λx.ab] -> S[λx.a][λx.b]
      ```

  - For example:

    ```
    {x} = go x
    [x] = abs x

    {λy.λx.xy}
      {λx.xy}
        [λx.xy]
          (S[λx.x][λx.y])
          (S(SKK)[λx.y])
          (S(SKK)(Ky))
        (S(SKK)(Ky))
      [λy.S(SKK)(Ky)]
        (S[λy.S(SKK)][λy.Ky])
        (S(K(S(SKK)))[λy.Ky])
        (S(K(S(SKK)))(S[λy.K][λy.y]))
        (S(K(S(SKK)))(S(KK)[λy.y]))
        (S(K(S(SKK)))(S(KK)(SKK)))
      (S(K(S(SKK)))(S(KK)(SKK)))
    (S(K(S(SKK)))(S(KK)(SKK)))
    ```
-}
naiveBracket :: Exp b a -> Out (SK a)
naiveBracket = go
 where
  go :: Exp b a -> Out (SK a)
  go = \case
    Var x    -> Var (V x)
    (:@) x y -> go x :@ go y
    Lam  _ b -> abs $ fmap sequence $ go $ fromScope b

  abs :: Out (Var b (SK a)) -> Out (SK a)
  abs = \case
    Lam b  _  -> absurd b
    Var (B _) -> VS :@ VK :@ VK
    Var (F v) -> VK :@ Var v -- Same as below but makes match exhaustive.
    Cns b     -> VK :@ b
    x   :@ y  -> VS :@ abs x :@ abs y


-- John Tromp's Bracket Abtraction Approach ------------------------------------

{-
  Bracket Abstraction -- John Tromp's translation from lambdas to
  combinators. It adds some optimizations.

  - The `go` routine is the same as the naive version.

  - The `abs` routine does some additional simplifications

    ```
    1) [λx.SKb]    -> SK
    2) [λx.x]      -> SKK
    3) [λx.m]      -> Km             --  m does not reference x
    4) [λx.mx]     -> m              --  m does not reference x
    5) [λx.xmx]    -> [λx.SSKxm]
    6) [λx.m(nl)]  -> [λx.S{λ_.m}nl] --  m,n are in {S,K}
    7) [λx.(mn)l]  -> [λx.Sm{λ_.l}n] --  m,l are in {S,K}
    8) [λx.mb(np)] -> [λx.Smnb]      --  m,n are in {S,K} ∧ b≡p
    9) [λx.mn]     -> S[λx.m][λx.n]
    ```
-}
johnTrompBracket :: (Eq b, Eq a) => Exp b a -> Out (SK a)
johnTrompBracket = go
 where
  go :: (Eq a, Eq b) => Exp b a -> Out (SK a)
  go = \case
    Var x    -> Var (V x)
    x   :@ y -> go x :@ go y
    Lam _  b -> abs $ fmap sequence $ go $ fromScope b

  lam :: (Eq a, Eq v, Eq w) => Out (Var v (SK a)) -> Out (Var w (SK a))
  lam = fF . abs

  fF :: Functor f => f a -> f (Var b a)
  fF = fmap F

  abs :: (Eq a, Eq b) => Out (Var b (SK a)) -> Out (SK a)
  abs = \case
    Lam b  _                          -> absurd b
    FS :@ FK :@ _                     -> VS :@ VK
    Var (B _)                         -> VS :@ VK :@ VK
    Var (F v)                         -> VK :@ Var v
    Cns b                             -> VK :@ b
    Cns m :@ VB _                     -> m
    VB v :@ m :@ VB _                 -> abs (FS :@ FS :@ FK :@ VB v :@ m)
    CB m          :@ (CB n :@ l)      -> abs (FS :@ lam m :@ n :@ l)
    (CB m :@ n)   :@ CB l             -> abs (FS :@ m :@ lam l :@ n)
    (CB m :@ b) :@ (CB n :@ p) | b==p -> abs (FS :@ m :@ n :@ b)
    x   :@ y                          -> VS :@ abs x :@ abs y

outToUruk :: Uruk p => Out (SK p) -> IO p
outToUruk = go
 where
  go = \case
    Lam b _   -> absurd b
    Var S     -> pure uEss
    Var K     -> pure uKay
    Var (V p) -> pure p
    x :@ y    -> join (uApp <$> go x <*> go y)
