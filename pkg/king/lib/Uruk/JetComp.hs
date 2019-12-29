module Uruk.JetComp where

import ClassyPrelude hiding (try)

import GHC.Natural  (Natural)
import Uruk.JetDemo (Ur((:@)))

import qualified Uruk.JetDemo as Ur


-- Types -----------------------------------------------------------------------

type Nat = Natural

data Exp = Lam Exp | Var Nat | Go Exp Exp | Prim Ur

data Deb = Zero | Succ Deb | DPrim Ur | Abs Deb | App Deb Deb

infixl 5 :#

data Com = Com :# Com | P Ur | I | K | S | Sn Nat | C | Cn Nat | B | Bn Nat

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
    (0, n)             -> Bn n :# x :# y
    (n, 0)             -> Cn n :# x :# y
    (n, m) | n == m    -> Sn n :# x :# y
           | n < m     ->                Bn (m - n) :# (Sn n :# x) :# y
           | otherwise -> Cn (n - m) :# (Bn (n - m) :#  Sn m :# x) :# y

{-
  Compile Oleg's combinators to jetted Ur combinators.
-}
ur :: Com -> Ur
ur = \case
    Bn 1        -> Ur.B
    Cn 1        -> Ur.C
    Sn 1        -> Ur.S
    Bn n        -> Ur.BLin n
    Cn n        -> Ur.CLin n
    Sn n        -> Ur.SLin n
    x :# y      -> ur x :@ ur y
    B           -> Ur.B
    C           -> Ur.C
    S           -> Ur.S
    I           -> Ur.I
    K           -> Ur.K
    P p         -> p
