module Ur.PicoMoloch where

import Ur.Common hiding (flat, A)
import Data.Flat        (Flat, flat, unflat)
import Control.Lens     (view, from)

type N = Atom

infixl 5 :@

data E = E :@ E | S | K | I | Lit N | Inc | Fol | Box | Opn | Run | Evl | Dum
  deriving (Eq, Ord, Generic)
  deriving anyclass (Flat)

data V = F E (V → V) | N N | B V | X E

valExp (F e _) = e
valExp (N n)   = Lit n
valExp (B x)   = Box :@ valExp x
valExp (X e)   = e

eval :: E -> V
eval = \case
    f :@ x → c (eval f) (eval x)
    S      → fn3 S \x y z → x `c` z `c` (y `c` z)
    K      → fn2 K const
    I      → fn1 I id
    Lit n  → N n
    Inc    → fn1 Inc inc
    Fol    → fn1 Fol fol
    Box    → fn1 Box jet
    Opn    → fn1 Opn opn
    Run    → fn2 Run run
    Evl    → fn1 Evl evl
    Dum    → fn1 Dum dum
  where
    c (F _ f) x = f x
    c f       x = X (valExp f :@ valExp x)

    fn1 e f = F e
         \x → traceFn e [x] (f x)

    fn2 e f = F e
         \x → F (e :@ valExp x)
         \y → traceFn e [x,y] (f x y)

    fn3 e f = F e
         \x → F (e :@ valExp x)
         \y → F (e :@ valExp x :@ valExp y)
         \z → traceFn e [x,y,z] (f x y z)

    jet (F e f) | e==add = B (fn2 e addJet)
    jet v                = B v

    inc (N n) = trace "INC" $ N (succ n)
    inc x     = X (Inc :@ valExp x)

    fol (N n) = trace "FOL" $ lNat n
    fol v     = X (Fol :@ valExp v)

    lNat 0 = fn2 (S :@ K) \i z → z
    lNat n = lSucc `c` lNat (pred n)
      where lSucc = fn3 (S :@ (S :@ (K :@ S) :@ K))
                        (\n i z → i `c` (n `c` i `c` z))

    opn (B x) = x
    opn v     = X (Opn :@ valExp v)

    run (B (F _ f)) x = f x
    run jf          x = X (Run :@ valExp jf :@ valExp x)

    dum = N . view (from atomBytes) . flat . encode . valExp

    evl (N (unflat . view atomBytes -> Right x)) = eval (decode x)
    evl v                                        = X (Evl :@ valExp v)

traceFn :: E -> [V] -> V -> V
traceFn e vs = ( \x
               → let inp = "(" <> intercalate " " (show e : fmap show vs) <> ")"
                 in trace (showSub inp (show x)) x
               )

showSub :: String -> String -> String
showSub inp oup = inp <> pad <> " --> " <> oup
  where
    pad = replicate (max 0 (65 - length inp)) ' '

--  add = \x y → Fol x Inc y
--  add = s :@ Fol :@ (k :@ Inc)
--  s i (k (s (s (k s) k)))
add ∷ E
add = S :@ Fol :@ (K :@ Inc)

--  add3 = \x -> Fol x Inc 3
--  add3 = (s (s % (k +)) (k 3))
addThree ∷ E
addThree = S :@ (S :@ Fol :@ (K :@ Inc)) :@ (K :@ Lit 3)


{-
Sxyz = xy(yz)
Sxyz = % (k+) (k+0)
-}

addJet ∷ V → V → V
addJet (N x) (N y) = trace "ADD" $ N (x+y)
addJet x     y     = trace "ADD" $ X (add :@ valExp x :@ valExp y)


-- Debug Output ----------------------------------------------------------------

instance Show E where
    show = \case
        e@(_ :@ _) → "(" <> intercalate "" (show <$> appList [] e) <> ")"
        Lit n      → show n
        S          → "s"
        K          → "k"
        I          → "i"
        Inc        → "+"
        Fol        → "%"
        Box        → "@"
        Opn        → "^"
        Run        → "!"
        Evl        → "?"
        Dum        → "_"
      where
        appList acc (f :@ x) = appList (x:acc) f
        appList acc x        = x:acc

instance Show V where
  show = \case
    B v   → "[" <> show v <> "]"
    N n   → show n
    F e _ → show e
    X e   → "ERR: " <> show e


-- Normalization (Redundant, for testing) --------------------------------------

simp ∷ E → E
simp =
    \x -> trace ("\t" <> show x) (go x)
  where
   go x = let res = bo x in if (res /= x)
                            then trace (showSub (show x) (show res)) res
                            else res
   bo = \case
    S      → S
    K      → K
    I      → I
    Lit n  → Lit n
    Inc    → Inc
    Fol    → Fol
    Box    → Box
    Opn    → Opn
    Run    → Run
    Evl    → Evl
    Dum    → Dum

    S     :@ x → S :@ go x
    K     :@ x → K :@ go x
    I     :@ x → go x
    Lit n :@ x → Lit n :@ x
    Inc   :@ x → go x & \case { Lit n → Lit (succ n);  xv → Inc :@ xv }
    Fol   :@ x → go x & \case { Lit n -> lNat n;      xv  → Fol :@ xv }
    Box   :@ x → Box :@ go x
    Opn   :@ x → go x & \case { Box :@ x → x; sf → Opn :@ sf }
    Run   :@ x → Run :@ go x
    Dum   :@ x → Lit $ view (from atomBytes) $ flat $ encode $ go x
    Evl   :@ x → go x & \case
                     Lit (unflat . view atomBytes -> Right x) → go (decode x)
                     xv                                       → Evl :@ xv

    Run :@ b :@ x → go b & \case Box :@ f -> go (f :@ x)
                                 sb       -> Box :@ sb :@ go x

    K :@ x :@ _ → go x
    S :@ x :@ y → S :@ go x :@ go y

    I :@ x :@ y → go x :@ go y

    S :@ K :@ _ :@ x → go x
    S :@ x :@ y :@ z → go (go x :@ z' :@ (go y :@ z')) where z' = go z

    x :@ y :@ z → case go (x :@ y) of
                    res | x :@ y == res -> res :@ go z
                    res                 -> go (res :@ z)

   lNat 0 = S :@ K
   lNat n = S :@ (S :@ (K :@ S) :@ K) :@ lNat (pred n)


-- Optimized Flat-Encoding for Expressions -------------------------------------

data E1
    = EA E1 E1
    | E2 E2
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Flat)

data E2
    = ES
    | EK
    | EI
    | E3 E3
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Flat)

data E3
    = ELit N
    | EInc
    | EFol
    | EBox
    | EOpn
    | ERun
    | EEvl
    | EDum
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (Flat)

encode :: E -> E1
encode = go
  where
    go = \case
        x :@ y -> EA (go x) (go y)
        S      -> E2 ES
        K      -> E2 EK
        I      -> E2 EI
        Lit n  -> E2 $ E3 (ELit n)
        Inc    -> E2 $ E3 EInc
        Fol    -> E2 $ E3 EFol
        Box    -> E2 $ E3 EBox
        Opn    -> E2 $ E3 EOpn
        Run    -> E2 $ E3 ERun
        Evl    -> E2 $ E3 EEvl
        Dum    -> E2 $ E3 EDum

decode :: E1 -> E
decode = go
  where
    go = \case
        EA x y           -> go x :@ go y
        E2 ES            -> S
        E2 EK            -> K
        E2 EI            -> I
        E2 (E3 (ELit n)) -> Lit n
        E2 (E3 EInc)     -> Inc
        E2 (E3 EFol)     -> Fol
        E2 (E3 EBox)     -> Box
        E2 (E3 EOpn)     -> Opn
        E2 (E3 ERun)     -> Run
        E2 (E3 EEvl)     -> Evl
        E2 (E3 EDum)     -> Dum


-- Pairs -----------------------------------------------------------------------

lCons :: E
lCons = S:@(K:@(S:@(K:@(S:@(K:@(S:@(K:@(S:@S:@(K:@K))):@K)):@S)):@(S:@(S:@K:@K)))):@K

lCar :: E
lCar = S :@ I :@ (K :@ K)

lCdr :: E
lCdr = S :@ I :@ (K :@ (S :@ K))
