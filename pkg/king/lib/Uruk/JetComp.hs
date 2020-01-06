module Uruk.JetComp where

import ClassyPrelude hiding (try)

import Numeric.Natural  (Natural)
import Numeric.Positive (Positive)
import Uruk.Lazy     (Ur, UrPoly((:@)))

import qualified Uruk.Lazy as Ur


-- Types -----------------------------------------------------------------------

type Nat = Natural
type Pos = Positive

data Exp = Lam Exp | Var Nat | Go Exp Exp | Prim Ur

data Deb = Zero | Succ Deb | DPrim Ur | Abs Deb | App Deb Deb

infixl 5 :#

data Com = Com :# Com | P Ur | I | K | S | Sn Pos | C | Cn Pos | B | Bn Pos

instance Show Com where
    show = \case
        S    → "s"
        I    → "i"
        C    → "c"
        K    → "k"
        B    → "b"
        P p  → "[" <> show p <> "]"
        Sn i → "s" <> show i
        Bn i → "b" <> show i
        Cn i → "c" <> show i
        x:#y → "(" <> intercalate " " (show <$> foldApp x [y]) <> ")"
      where
        foldApp (x :# y) acc = foldApp x (y:acc)
        foldApp x        acc = x : acc


--------------------------------------------------------------------------------

add = Prim Ur.Add
inc = Prim Ur.Inc
cons = Prim Ur.Con
pak = Prim Ur.Pak

nat n = Prim (Ur.Nat n)

try ∷ Exp → Ur
try = ur . snd . ski . expDeb

expDeb ∷ Exp → Deb
expDeb = go
  where
    go (Lam x)  = Abs (go x)
    go (Var x)  = peano x
    go (Go x y) = App (go x) (go y)
    go (Prim p) = DPrim p
    peano 0 = Zero
    peano n = Succ (peano (pred n))


-- Oleg's Combinators ----------------------------------------------------------

{-
    Compile lambda calculus to combinators.
-}
ski :: Deb -> (Nat, Com)
ski deb = case deb of
  DPrim p                        -> (0,       P p)
  Zero                           -> (1,       I)
  Succ d    | x@(n, _) <- ski d  -> (n + 1,   f (0, K) x)
  App d1 d2 | x@(a, _) <- ski d1
            , y@(b, _) <- ski d2 -> (max a b, f x y)
  Abs d | (n, e) <- ski d -> case n of
                               0 -> (0,       K :# e)
                               _ -> (n - 1,   e)
  where
  f (a, x) (b, y) = case (a, b) of
    (0, 0)             ->         x :# y
    (0, n)             -> Bn (p n) :# x :# y
    (n, 0)             -> Cn (p n) :# x :# y
    (n, m) | n == m    -> Sn (p n) :# x :# y
           | n < m     ->                 Bn(p(m-n)) :# (Sn (p n) :# x) :# y
           | otherwise -> Cn (p(n-m)) :# (Bn(p(n-m)) :#  Sn (p m) :# x) :# y

  p ∷ Natural → Positive
  p = fromIntegral

{-
  Compile Oleg's combinators to jetted Ur combinators.
-}
ur :: Com -> Ur
ur = \case
    Bn 1        -> Ur.B
    Cn 1        -> Ur.C
    Sn 1        -> Ur.S
    Bn n        -> Ur.fast $ Ur.Bn n
    Cn n        -> Ur.fast $ Ur.Cn n
    Sn n        -> Ur.fast $ Ur.Sn n
    x :# y      -> ur x :@ ur y
    B           -> Ur.B
    C           -> Ur.C
    S           -> Ur.S
    I           -> Ur.I
    K           -> Ur.K
    P p         -> p
