module Uruk.JetComp where

import ClassyPrelude hiding (try)

import Numeric.Natural  (Natural)
import Numeric.Positive (Positive)
import Uruk.JetDemo     (Ur, UrPoly((:@)))

import qualified Uruk.JetDemo as Ur


-- Types -----------------------------------------------------------------------

type Nat = Natural
type Pos = Positive

data Exp = Lam Exp | Var Nat | Go Exp Exp | Prim Ur

data Deb = Zero | Succ Deb | DPrim Ur | Abs Deb | App Deb Deb

infixl 5 :#

data Com = Com :# Com | P Ur | I | K | S | Sn Pos | C | Cn Pos | B | Bn Pos

instance Show Exp where
  show = \case
    Lam exp → "\\" <> show exp
    Var n   → "$" <> show n
    Prim p  → show p
    Go x y  → "(" <> intercalate " " (show <$> apps x [y]) <> ")"
      where
        apps (Go f x) xs = apps f (x:xs)
        apps exp      xs = exp : xs

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

moon ∷ Exp → Ur
moon = ur . snd . ski . expDeb

moonStrict ∷ Exp → Ur
moonStrict = moon . strict

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


-- Strict ----------------------------------------------------------------------

{-
    Uses `seq` to prevent overly-eager evaluation.

    Without this, `\x -> exensive 9` would compile to `K (expensive
    9)` which normalizes to `K result`. We want to delay the evaluation
    until the argument is supplied, so we instead produce:

        \x → (seq x expensive) 9

    This transformation is especially important in recursive code,
    which will not terminate unless the recursion is delayed.

    TODO Optimize using function arity.
        `(kk)` doesn't need to become `Q0kk` since the head (`k`)
        is undersaturated.
-}
strict ∷ Exp → Exp
strict = top
  where
    top = \case
      Lam b  → Lam (go b)
      Var n  → Var n
      Prim p → Prim p
      Go f x → Go (top f) (top x)

    go = \case
      Lam b  → Lam (go b)
      Var n  → Var n
      Prim p → Prim p
      Go f x → case (go f, go x) of
          (fv, xv) | dep fv || dep xv → Go fv xv
          (fv, xv)                    → Go (addDep fv) xv

addDep ∷ Exp → Exp
addDep = \case
    Go f x → Go (addDep f) x
    exp    → Prim Ur.Seq `Go` Var 0 `Go` exp

dep ∷ Exp → Bool
dep = go 0
  where
    go v = \case
        Lam b        → go (succ v) b
        Var n | v==n → True
        Var n        → False
        Prim p       → False
        Go f x       → go v f || go v x

{-
    ~/  %ack  2
    %-  fix
    |=  (ack m n)
    ?:  (zer m)  (inc n)
    ?:  (zer n)  (ack (fec m) 1)
    (ack (fec m) (ack m (fec n)))
-}

infixl 5 %

(%) = Go

iff ∷ Exp → Exp → Exp → Exp
iff c t e =
    (Prim Ur.Iff % c % t' % e') % Prim Ur.Uni
  where
    (t',e') = (abstract t, abstract e)

    abstract ∷ Exp → Exp
    abstract = Lam . go 0
      where
        go d = \case
          Prim p        → Prim p
          Go x y        → Go (go d x) (go d y)
          Var n | n < d → Var n
          Var n         → Var (succ n)
          Lam b         → Lam (go (succ d) b)

acker ∷ Exp
acker =
    let fec = \x → Prim Ur.Fec % x
        zer = \x → Prim Ur.Zer % x
        inc = \x → Prim Ur.Inc % x
        fix = Prim Ur.Fix
        go  = Var 2
        m   = Var 1
        n   = Var 0
    --  err = Prim Ur.Ded % Prim Ur.K
    in
        fix % (
            Lam $ Lam $ Lam $
            iff (zer m) (inc n) $
            iff (zer n) (go % fec m % nat 1) $
            go % fec m % (go % m % fec n)
        )

toZero ∷ Exp
toZero =
    let fec = \x → Prim Ur.Fec % x
        zer = \x → Prim Ur.Zer % x
        fix = Prim Ur.Fix
        go  = Var 1
        x   = Var 0
    in
        fix % (
            Lam $ Lam $
            iff (zer x) x (go % fec x)
        )
