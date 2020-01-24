{-
    Jet's don't work in the simplifier atm, for the usual reason. `add`
    is curried, and it doesn't make sense to jet curried functions.

    It *does* work in the interpreter, though, which is interesting.

    The interpreter turns the jet into a haskell function at jet-matching
    time, and tracks applied arguments until it has enough to run the jet.
    It keeps track of what the AST should be as it goes along, and can
    still reproduce it if needed.

    This seems like a hack, though. We can only do that because we know
    a-priori that the jet takes two arguments. If we wanted to optimize
    the jet call, we wouldn't know how many arguments we should consume
    before executing. Right?

    Even if it *could* work, it's still a bad idea. Semantically,
    we're transforming the jetted code, which means that, if we round-trip
    the AST to disk, it wont jet match when it comes back.
-}

module Ur.PicoMoloch where

import Ur.Common hiding (flat, A, trace, traceShow)
import Data.Flat        (Flat, flat, unflat)
import Control.Lens     (view, from)
import Debug.Trace      (trace, traceShow)

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

    jet (F e f) | simp e==addRaw = B (fn2 e addJet)
    jet (F e f)                  = traceShow ("MISMATCH", e, add)
                                 $ B (fn2 e addJet)
    jet v                        = B v

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

    dum = N . bytesAtom . flat . enc . valExp

    evl (N (unflat . atomBytes -> Right x)) = eval (dec x)
    evl v                                        = X (Evl :@ valExp v)

    addJet (N x) (N y) = trace "ADD" $ N (x+y)
    addJet x     y     = trace "ADD" $ X (add :@ valExp x :@ valExp y)


-- Jets ------------------------------------------------------------------------

{-
    jet     = \f x -> Run (Box f) x
    add     = \x y → Fol x Inc y
    con     = \x y f → f x y
    car     = \p -> p (\x y -> x)
    cdr     = \p -> p (\x y -> y)
    uncurry = \f x y -> f (cons x y)
    curry   = \p -> p (\x y -> y)
-}
addRaw = S :@ Fol :@ (K :@ Inc)
conRaw = S:@(K:@(S:@(K:@(S:@(K:@(S:@(K:@(S:@S:@(K:@K))):@K)):@S)):@(S:@(S:@K:@K)))):@K
carRaw = S :@ I :@ (K :@ K)
cdrRaw = S :@ I :@ (K :@ (S :@ K))

uncurryRaw = S :@ (S :@ (K :@ (S:@(K:@(S:@(K:@S):@K)) :@ S)) :@ K) :@ (K:@con)

curryRaw = S :@ (S:@(K :@ S) :@ (S:@(S:@(K:@S):@K):@(K:@car))) :@ (K :@ cdr)

jet = S :@ (K :@ Run) :@ Box
add = simp $ jet :@ addRaw
con = simp $ jet :@ conRaw
cdr = simp $ jet :@ cdrRaw
car = simp $ jet :@ carRaw
cur = simp $ jet :@ uncurryRaw
unc = simp $ jet :@ curryRaw


-- Debug Output ----------------------------------------------------------------

traceFn :: E -> [V] -> V -> V
traceFn e vs = ( \x
               → let inp = "(" <> intercalate " " (show e : fmap show vs) <> ")"
                 in trace (showSub inp (show x)) x
               )

showSub :: String -> String -> String
showSub inp oup = inp <> pad <> " --> " <> oup
  where
    pad = replicate (max 0 (65 - length inp)) ' '

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
        Lit n :@ x → Lit n :@ go x
        Inc   :@ x → go x & \case { Lit n → Lit (succ n);  xv → Inc :@ xv }
        Fol   :@ x → go x & \case { Lit n → go (lNat n);   xv → Fol :@ xv }
        Box   :@ x → Box :@ go x
        Opn   :@ x → go x & \case { Box :@ x → go x; sf → Opn :@ sf }
        Run   :@ x → Run :@ go x
        Dum   :@ x → Lit $ bytesAtom $ flat $ enc $ go x
        Evl   :@ x → go x & \case
                         Lit (unflat . atomBytes -> Right x) → go (dec x)
                         xv                                  → Evl :@ xv

        K   :@ x :@ _ → go x
        S   :@ K :@ _ → I
        S   :@ x :@ y → S :@ go x :@ go y
        I   :@ x :@ y → go (go x :@ go y)

        Run :@ b :@ x → go b & \case Box :@ f -> let (xv, fv) = (go x, go f)
                                                 in if xv == addRaw
                                                    then trace "ADD" $
                                                           go (fv :@ xv)
                                                    else go (fv :@ xv)
                                     bv       -> Box :@ bv :@ go x

        S :@ K :@ _ :@ x → go x
        S :@ x :@ y :@ z → go (go x :@ z' :@ (go y :@ z')) where z' = go z

        x :@ y :@ z → let (xv, yv, zv) = (go x, go y, go z)
                      in case go (xv :@ yv) of
                           res | x :@ y == res -> trace "ABORT" $ res :@ zv
                           res                 -> go (res :@ zv)

    lNat 0 = S :@ K
    lNat n = S :@ (S :@ (K :@ S) :@ K) :@ lNat (pred n)

    -- addJet ∷ E → E → E
    -- addJet (Lit x) (Lit y) = trace "ADD" $ Lit (x+y)
    -- addJet x       y       = trace "ADD" $ (addRaw :@ x :@ y)


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

enc :: E -> E1
enc = go
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

dec :: E1 -> E
dec = go
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
