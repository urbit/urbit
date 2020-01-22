module Uruk.JetComp where

import ClassyPrelude hiding (try)

import Numeric.Natural  (Natural)
import Numeric.Positive (Positive)
import Uruk.JetDemo     (Ur, UrPoly((:@), Fast))
import Control.Arrow ((>>>))

import qualified Uruk.JetDemo as Ur


-- Types -----------------------------------------------------------------------

type Nat = Natural
type Pos = Positive

data Exp
    = Lam Exp
    | Var Nat
    | Go Exp Exp
    | Prim Ur
    | Loop Exp
    | If Exp Exp Exp
    | Case Exp Exp Exp
    | Jet Pos Nat Exp

instance Num Exp where
    negate      = error "hack"
    (+)         = error "hack"
    (*)         = error "hack"
    fromInteger = Var . fromIntegral
    abs         = error "hack"
    signum      = error "hack"

data Deb = Zero | Succ Deb | DPrim Ur | Abs Deb | App Deb Deb
  deriving Eq

infixl 5 :#

data Com = Com :# Com | P Ur | I | K | S | Sn Pos | C | Cn Pos | B | Bn Pos

instance Show Exp where
  show = \case
    Lam exp    → "<" <> show exp <> ">"
    Var n      → "$" <> show n
    Prim p     → filter (\x → x/='{' && x/='}' && x/='#') (show p)
    Loop x     → "..(" <> show x <> ")"
    If c t e   → "?:(" <> show c <> " " <> show t <> " " <> show e <> ")"
    Case x l r → "?-(" <> show x <> "; " <> show l <> " " <> show r <> ")"
    Jet n t b  → "~/(" <> show n <> " " <> show t <> " " <> show b <> ")"
    Go x y     → "(" <> intercalate " " (show <$> apps x [y]) <> ")"
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

plus ∷ Exp → Exp → Exp
plus x y = Prim Ur.Add % x % y

mul ∷ Exp → Exp → Exp
mul x y = Prim Ur.Mul % x % y

moon ∷ Exp → Ur
moon = ur . snd . ski . expDeb

moonStrict ∷ Exp → Ur
moonStrict = ur . snd . ski . strict . expDeb

expDeb ∷ Exp → Deb
expDeb = go
  where
    go (Lam x)  = Abs (go x)
    go (Var x)  = peano x
    go (Go x y) = App (go x) (go y)
    go (Prim p) = DPrim p

    go (Jet n t b) = DPrim (Ur.J n)
                       `App` DPrim (Ur.Nat t)
                       `App` go b

    go (Loop x) = DPrim Ur.Fix
                    `App` (Abs $ go x)

    go (Case x l r) = DPrim Ur.Cas
                        `App` go x
                        `App` go l
                        `App` go r

    go (If c t e) = DPrim Ur.Iff
                      `App` go c
                      `App` go (wrapLam t)
                      `App` go (wrapLam e)

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
  Compile Oleg's combinators to jetted Ur combinators.  -}
ur :: Com -> Ur
ur = \case
    Bn 1        -> Ur.B
    Cn 1        -> Ur.C
    Sn 1        -> Ur.S
    Bn n        -> Ur.Fast (fromIntegral $ n+2) (Ur.Bn n) []
    Cn n        -> Ur.Fast (fromIntegral $ n+2) (Ur.Cn n) []
    Sn n        -> Ur.Fast (fromIntegral $ n+2) (Ur.Sn n) []
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
strict ∷ Deb → Deb
strict = top
  where
    top = \case
        Abs b   → Abs (go b)
        Zero    → Zero
        Succ n  → Succ n
        DPrim p → DPrim p
        App f x → App (top f) (top x)

    go ∷ Deb → Deb
    go = \case
        Abs b   → Abs (go b)
        Zero    → Zero
        Succ n  → Succ n
        DPrim p → DPrim p
        App f x → case (go f, go x) of
            (fv, xv) | dep fv || dep xv → App fv xv
            (fv, xv)                    → App (addDep fv) xv

addDep ∷ Deb → Deb
addDep = \case
    App f x → App (addDep f) x
    exp     → DPrim Ur.Seq `App` Zero `App` exp

dep ∷ Deb → Bool
dep = go Zero
  where
    go v = \case
        Abs b             → go (Succ v) b
        x@(Zero)   | x==v → True
        x@(Succ _) | v==v → True
        Zero              → False
        Succ _            → False
        DPrim p           → False
        App f x           → go v f || go v x

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
    Prim Ur.Iff % c % t' % e'
  where
    (t',e') = (wrapLam t, wrapLam e)

wrapLam ∷ Exp → Exp
wrapLam = Lam . abstract

abstract ∷ Exp → Exp
abstract = go 0
  where
    go d = \case
      Prim p        → Prim p
      Go x y        → Go (go d x) (go d y)
      Var n | n < d → Var n
      Var n         → Var (succ n)
      Lam b         → Lam (go (succ d) b)
      Jet n t b     → Jet n t (go d b)
      Case c l r    → Case (go d c) (go (succ d) l) (go (succ d) r)
      Loop x        → Loop (go (succ d) x)
      If c t e      → If (go d c) (go d t) (go d e)

acker ∷ Exp
acker =
    let fec = \x → Prim Ur.Fec % x
        zer = \x → Prim Ur.Zer % x
        inc = \x → Prim Ur.Inc % x
        go  = Var 2
        m   = Var 1
        n   = Var 0
    --  err = Prim Ur.Ded % Prim Ur.K
    in
        (
            Jet 2 99 $ Loop $ Lam $ Lam $
            If (zer m) (inc n) $
            If (zer n) (go % fec m % nat 1) $
            go % fec m % (go % m % fec n)
        )

{-
    (if c t e) α  => if c (Lam(t $0 α)) (Lam(e $0 α))
    fix f α       => fix (Lam(f $0 α))
    (cas x l r) α => cas x (Lam(l $0 α)) (Lam(r $0 α))
-}

toZero ∷ Exp
toZero =
    let fec = \x → Prim Ur.Fec % x
        zer = \x → Prim Ur.Zer % x
        go  = Var 1
        x   = Var 0
    in
        Jet 1 99 $ Loop $ Lam $
          If (zer x) x (go % fec x)

toBody ∷ Exp
toBody =
    let fec = \x → Prim Ur.Fec % x
        zer = \x → Prim Ur.Zer % x
        go  = Var 1
        x   = Var 0
    in
        Jet 1 99 $ Lam $ Lam $
          If (zer x) x (go % fec x)

pattern L x = Lam x
pattern V n = Var n

sLam 1 =                 L $ L $ L ((2%0) % (1%0))
sLam 2 =             L $ L $ L $ L ((3%1%0) % (2%1%0))
sLam 3 =         L $ L $ L $ L $ L ((4%2%1%0) % (3%2%1%0))
sLam 4 =     L $ L $ L $ L $ L $ L ((5%3%2%1%0) % (4%3%2%1%0))
sLam 5 = L $ L $ L $ L $ L $ L $ L ((6%4%3%2%1%0) % (5%4%3%2%1%0))
sLam n = error ("TODO: Sn " <> show n)

bLam 1 =             L $ L $ L (2 % (1%0))
bLam 2 =         L $ L $ L $ L (3 % (2%1%0))
bLam 3 =     L $ L $ L $ L $ L (4 % (3%2%1%0))
bLam 4 = L $ L $ L $ L $ L $ L (5 % (4%3%2%1%0))
bLam n = error ("TODO: Bn " <> show n)

cLam 1 =                 L $ L $ L ((2%0) % 1)
cLam 2 =             L $ L $ L $ L ((3%1%0) % 2)
cLam 3 =         L $ L $ L $ L $ L ((4%2%1%0) % 3)
cLam 4 =     L $ L $ L $ L $ L $ L ((5%3%2%1%0) % 4)
cLam 5 = L $ L $ L $ L $ L $ L $ L ((6%4%3%2%1%0) % 5)
cLam n = error ("TODO: Cn " <> show n)

step ∷ Exp → Maybe Exp
step = \case

    --  Recognize loops
    Prim Ur.Fix `Go` exp →
        Just (Loop (abstract exp `Go` Var 0))

    --  Recognize if expressions
    Prim Ur.Iff `Go` c `Go` t `Go` e →
        Just $ If c (t `Go` Prim Ur.Uni)
                    (e `Go` Prim Ur.Uni)

    --  Prim to Lambda
    Prim Ur.Seq                   → Just (L (L 0))
    Prim Ur.K                     → Just (L (L 1))
    Prim Ur.S                     → Just (sLam 1)
    Prim Ur.I                     → Just (L 0)
    Prim Ur.B                     → Just (bLam 1)
    Prim Ur.C                     → Just (cLam 1)
    Prim (Ur.Fast _ (Ur.Sn n) []) → Just (sLam n)
    Prim (Ur.Fast _ (Ur.Bn n) []) → Just (bLam n)
    Prim (Ur.Fast _ (Ur.Cn n) []) → Just (cLam n)

    --  Traversal
    Lam x                  → Lam <$> step x
    Var n                  → Nothing
    Prim p                 → Nothing
    Loop e                 → Loop <$> step e
    If (step→Just c) t e   → Just $ If c t e
    If c (step→Just t) e   → Just $ If c t e
    If c t (step→Just e)   → Just $ If c t e
    If _ _ _               → Nothing
    Case (step→Just x) l r → Just $ Case x l r
    Case x (step→Just l) r → Just $ Case x l r
    Case x l (step→Just r) → Just $ Case x l r
    Case _ _ _             → Nothing
    Jet n t b              → Jet n t <$> step b

    --  Reduction
    Go (step→Just f) x            → Just (Go f x)
    Go f (step→Just x)            → Just (Go f x)
    Lam f `Go` x                  → Just (subst f x)
    Go f x                        → Nothing

subst ∷ Exp → Exp → Exp
subst =
    \e v → go 0 v e
  where
    abs = abstract

    go d v = \case
        Lam e        → Lam (go (d+1) (abs v) e)
        Var n | n==d → v
        Var n | n>d  → Var (pred n)
        Var n        → Var n
        Go x y       → Go (go d v x) (go d v y)
        Prim p       → Prim p
        Loop e       → Loop (go (d+1) (abs v) e)
        If c t e     → If (go d v c) (go (d+1) (abs v) t) (go (d+1) (abs v) e)
        Case x l r   → Case (go d v x) (go (d+1) (abs v) l) (go (d+1) (abs v) r)
        Jet n t b    → Jet n t (go d v b)


eval ∷ Exp → IO Exp
eval x = do
    -- print x
    case step x of
        Nothing → pure x
        Just x' → eval x'

stuff ∷ Exp
stuff =
    Lam $ Lam $
      let x = Var 1
          y = Var 0
      in
          plus x (mul x y)

-- Decompilation ---------------------------------------------------------------

{-
step ∷ Ur → Maybe Ur
step = \case
    Ur.K :@ x :@ y        → Just $ x
    (step → Just xv) :@ y → Just $ xv :@ y
    x :@ (step → Just yv) → Just $ x :@ yv
    Ur.S :@ x :@ y :@ z   → Just $ x :@ z :@ (y :@ z)
    Fast 0 u us           → runJet u us
    Fast 0 u us :@ x      → (Fast 0 u us :@) <$> step x
    Fast n u us :@ x      → Just $ Fast (pred n) u (us <> [x])
    _                     → Nothing
-}

{-
deCompile ∷ Nat → Ur → Exp
deCompile n = abs n . match . exp . go . free n . Ur.simp
  where
    free 0 x = undefined -- x
    free n x = undefined -- free (n-1) (x :@ Ur.Free (n-1))

    go (step → Just x) = go x
    go x               = x

    abs 0 x = x
    abs n x = abs (n-1) (Lam x)

    app ∷ Exp → [Exp] → Exp
    app f []     = f
    app f (x:xs) = app (f % x) xs

    int = fromIntegral

    match ∷ Exp → Exp
    match = traceShowId >>> \case
        Prim Ur.Fix `Go` x → Loop (toLambda x)
        x `Go` y           → match x `Go` match y
        Lam body           → Lam (match body)
        Prim p             → Prim p
        Var v              → Var v
        Loop x             → Loop (match x)
        If c t e           → If (match c) (match t) (match e)
        Case x l r         → If (match x) (match l) (match r)
-}

deCompile ∷ Ur → Exp
deCompile = Ur.simp >>> \case
    Ur.Fast _ (Ur.Slow n (Ur.Nat t) b) [] → Jet n t (toLambda (int n) (toExp b))
    ur                                    → toExp ur
  where
    int = fromIntegral

toLambda ∷ Nat → Exp → Exp
toLambda args = lam args . kal 0 . abs args
  where
    lam 0 e = e
    lam n e = lam (n-1) (Lam e)

    abs 0 e = e
    abs n e = abs (n-1) (abstract e)

    kal n e | n==args = e
    kal n e           = kal (succ n) (e % Var n)

int = fromIntegral

toExp ∷ Ur → Exp
toExp = go
  where
    go = \case
        (x :@ y)       → go x % go y
        Fast n u us    → foldl' (%) (Prim $ Fast (n + int(length us)) u [])
                                    (go <$> us)
        Ur.J n         → Prim (Ur.J n)
        Ur.K           → Prim Ur.K
        Ur.S           → Prim Ur.S
        Ur.D           → Prim Ur.D

runJet ∷ Ur.Jet → [Ur] → Maybe Ur
runJet = curry \case
    ( Ur.JSeq,        [x,y]   ) → Just y
    ( Ur.Slow n t b,  us      ) → Just $ go b us
    ( Ur.Wait _,      u:us    ) → Just $ go u us
    ( Ur.Eye,         [x]     ) → Just x
    ( Ur.Bee,         [f,g,x] ) → Just (f :@ (g :@ x))
    ( Ur.Sea,         [f,g,x] ) → Just (f :@ x :@ g)
    ( Ur.Bn _,        f:g:xs  ) → Just $ f :@ go g xs
    ( Ur.Cn _,        f:g:xs  ) → Just $ go f xs :@ g
    ( Ur.Sn _,        f:g:xs  ) → Just $ go f xs :@ go g xs
    ( Ur.JNat n,      [x,y]   ) → Just $ applyNat n x y
    ( Ur.JBol c,      [x,y]   ) → Just $ if c then x else y

    ( j,           xs      ) → Nothing
  where

    applyNat 0 i z = z
    applyNat n i z = i :@ (applyNat (pred n) i z)

    go ∷ Ur → [Ur] → Ur
    go acc = \case { [] → acc; x:xs → go (acc :@ x) xs }


{-
    (K α β)  ⇒  α
    (inc α)  ⇒  (inc α)  --  why is this different?

    Only things produced by the compiler should be reduced.
-}
