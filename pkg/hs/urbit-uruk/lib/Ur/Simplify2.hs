{-
    Evaluation Rules (over-complicated for now)

    *J      → J
    *(Ja)   → J (*x)
    *(Jan)  → J (*x) (*y)

    *(J(N0)nx)   → *
    *(J(N1)nf)   → (J(N1)(*n)(*f))
    *(J(N1)nfx)  → *(fx)
    *(J(N2)nf)   → (J(N2)(*n)(*f))
    *(J(N2)nfx)  → (J(N2)(*n)(*f)(*x))
    *(J(N2)nfxy) → *(fxy)
    *(Jxyz)      → *(J(*x)(*y)(*z))

    *K     → K
    *(Kx)  → K (*x)
    *(Kxy) → *x

    *S      → S
    *(Sx)   → S (*x)
    *(Sxy)  → S (*x) (*y)
    *(Sxyz) → (*x) (*z) *(y z)

    *D    → D
    *(Dx) → DUMP (*x)

    F   → F
    Fx  → F(*x)
    Fxy → *(x(Fx)y)

    N n       → N n
    Inc       → Inc
    Inc (N n) → N (n+1)
    Inc x     → Inc (*x)
    Fol       → Fol
    Fol (N 0) → SK
    Fol (N 1) → SKK
    Fol (N n) → (S(S(KS)K)) (Fol(N(n-1)))

    *(fx) → *((*f)(*x))
-}

module Ur.Simplify2 where

import Ur.CombExp hiding (Nat)
import Ur.Common  hiding (A, C, L, R, flat)

import Control.Lens (from, view)
import Data.Flat    (flat, Flat)
import GHC.Natural  (Natural)
import Ur.Noun      (bytesAtom)


-- Switch to List-Based Representation -----------------------------------------

type Nat = Natural

data P = S | K | I              --  SKI
       | J Natural Text         --  Jets
       | F                      --  Fixed-Point
       | N Natural | Inc | Fol  --  Naturals
       | D                      --  Expression unpacking.
       | U                      --  Unit
       | L | R | Case           --  Sum types
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData, Flat)

--  The arity of each built-in combinator.
tagPrim ∷ P → V P
tagPrim prim = V prim (arity prim) []
  where
    arity = \case
        S     → 3
        K     → 2
        I     → 1
        D     → 1
        U     → 1
        L     → 1
        R     → 1
        Case  → 3
        F     → 2
        N n   → 1
        Inc   → 1
        Fol   → 1
        J c n → c+1


--  This function will crash if combinators are simplified with the
--  wrong number of arguments.
simplifyPrim ∷ P → [V P] → X P
simplifyPrim = curry go
  where
    go ∷ (P, [V P]) → X P
    go ( S,     [z,y,x] ) = C x :@ C z :@ (C y :@ C z)
    go ( K,     [y,x]   ) = C x
    go ( D,     [x]     ) = C (tagPrim (dump x))
    go ( U,     [_]     ) = C iVal
    go ( L,     [_,x]   ) = C x
    go ( R,     [_,x]   ) = C x
    go ( Case,  [x,r,l] ) = C x :@ C uVal :@ C l :@ C r
    go ( F,     [x,f]   ) = C f :@ C (V F 1 [f]) :@ C x
    go ( N n,   [_]     ) = C $ foldNat n
    go ( Inc,   [x]     ) = C $ tagPrim $ N $ succ $ getNat x
    go ( Fol,   [x]     ) = C $ foldNat $ getNat x
    go ( J _ _, x:xs    ) = jetApp x xs
    go ( p    , ar      ) = error ("prim-no-simplify: " <> show (p,ar))

    iVal = tagPrim I
    uVal = tagPrim U

    getNat ∷ V P → Nat
    getNat (V (N n) _ _) = n
    getNat (V _     _ _) = error "bad-nat"

    jetApp ∷ V P → [V P] → X P
    jetApp f []     = C f
    jetApp x [f]    = C f :@ C x
    jetApp l (x:xs) = jetApp x xs :@ C l

    dump = N . bytesAtom . flat . cDump

eval ∷ E P → V P
eval = reEval . cPrep tagPrim

reEval ∷ X P → V P
reEval = cEval simplifyPrim

zer, one, inc ∷ V P
zer = eval (C S :@ C K)
one = eval (C I)
inc = eval (C S :@ (C S :@ (C K :@ C S) :@ C K))

foldNat ∷ Nat → V P
foldNat = reEval . go
  where go 0 = C zer
        go 1 = C one
        go n = C inc :@ C (foldNat $ pred n)
