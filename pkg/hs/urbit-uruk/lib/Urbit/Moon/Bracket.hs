{-
  Compile lambda calculus to SK combinators using "bracket abstraction".
-}
module Urbit.Moon.Bracket
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


-- Types -----------------------------------------------------------------------

infixl 5 :@;

data Exp p b a
  = Lam b (Scope b (Exp p b) a)
  | Exp p b a :@ Exp p b a
  | Pri p
  | Var a
 deriving (Functor, Foldable, Traversable)

data SK a = S | K | P a
 deriving (Eq, Ord, Show)

type Unit = ()

type Inp p a = Exp p Unit a
type Out p a = Exp p Void a


-- Instances -------------------------------------------------------------------

deriveEq1 ''Exp
deriveShow1 ''Exp

instance (NFData p, NFData b, NFData a) => NFData (Exp p b a) where
 rnf = \case
   Lam b s -> rnf b `seq` rnf s
   x :@ y -> rnf x `seq` rnf y
   Pri p -> rnf p
   Var a -> rnf a

instance (Eq p, Eq b, Eq a) => Eq (Exp p b a) where
  (==) = eq1

instance (Show p, Show b, Show a) => Show (Exp p b a) where
  showsPrec = showsPrec1

instance Monad (Exp p b) where
  return = Var
  Var a    >>= f = f a
  Pri p    >>= _ = Pri p
  Lam  v b >>= f = Lam v (b >>>= f)
  (:@) x y >>= f = (:@) (x >>= f) (y >>= f)

instance Applicative (Exp p b) where
  pure  = Var
  (<*>) = ap


-- Patterns --------------------------------------------------------------------

pattern PS = Pri S
pattern PK = Pri K

wrapSK :: Exp p b a -> Exp (SK p) b a
wrapSK = \case
  Pri p   -> Pri (P p)
  Var v   -> Var v
  x :@ y  -> wrapSK x :@ wrapSK y
  Lam b x -> Lam b $ toScope $ wrapSK $ fromScope x

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

pattern VB a = Var (B a)

isComb :: Exp (SK p) c a -> Maybe (SK q)
isComb = \case
  Pri S -> Just S
  Pri K -> Just K
  _     -> Nothing

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
naiveBracket :: forall p f. Eq f => Inp p f -> Out (SK p) f
naiveBracket = go . wrapSK
 where
  go :: Eq a => Inp (SK p) a -> Out (SK p) a
  go = \case
    Var x    -> Var x
    Pri p    -> Pri p
    (:@) x y -> go x :@ go y
    Lam  _ b -> abs $ go $ fromScope b

  abs :: Eq a => Out (SK p) (Var () a) -> Out (SK p) a
  abs = \case
    Lam b  _   -> absurd b
    Pri p      -> Pri p
    Var (B ()) -> PS :@ PK :@ PK
    Var (F v)  -> PK :@ Var v -- Same as below but makes match exhaustive.
    Cns b      -> PK :@ b
    x   :@ y   -> PS :@ abs x :@ abs y


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
johnTrompBracket :: forall p f. (Eq p, Eq f) => Inp p f -> Out (SK p) f
johnTrompBracket = go . wrapSK
 where
  go :: Eq a => Inp (SK p) a -> Out (SK p) a
  go = \case
    Var x    -> Var x
    Pri p    -> Pri p
    x   :@ y -> go x :@ go y
    Lam () b -> abs $ go $ fromScope b

  abs :: Eq a => Out (SK p) (Var () a) -> Out (SK p) a
  abs = \case
    Lam b  _                          -> absurd b
    PS :@ PK :@ _                     -> PS :@ PK
    Var (B ())                        -> PS :@ PK :@ PK
    Var (F v)                         -> PK :@ Var v
    Pri p                             -> PK :@ Pri p
    Cns b                             -> PK :@ b
    Cns m :@ VB ()                    -> m
    VB v :@ m :@ VB ()                -> abs (PS :@ PS :@ PK :@ VB v :@ m)
    CB m          :@ (CB n :@ l)      -> abs (PS :@ abs (Pri m) :@ Pri n :@ l)
    (CB m :@ n)   :@ CB l             -> abs (PS :@ Pri m :@ abs (Pri l) :@ n)
    (CB m :@ b) :@ (CB n :@ p) | b==p -> abs (PS :@ Pri m :@ Pri n :@ b)
    x   :@ y                          -> PS :@ abs x :@ abs y

outToUruk :: (p, p, p -> p -> p) -> Out (SK p) Void -> p
outToUruk (s,k,app) = go
 where
  go = \case
    Lam b _   -> absurd b
    Var v     -> absurd v
    Pri S     -> s
    Pri K     -> k
    Pri (P p) -> p
    x :@ y    -> app (go x) (go y)
