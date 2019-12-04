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

module Ur.Simplify where

import Ur.Common hiding (A, flat, succ)
import Ur.Lang

import Control.Concurrent (threadDelay)
import Control.Lens       (from, view)
import Data.Flat          (flat)
import GHC.Natural        (Natural)
import Noun               (atomBytes)
import System.IO.Unsafe   (unsafePerformIO)


-- Interpreter -----------------------------------------------------------------

simp ∷ E → E
simp = \case
    D      → D
    D :@ x → N $ view (from atomBytes) $ flat $ go x

    S                → S
    S :@ x           → S :@ go x
    S :@ x :@ y      → S :@ go x :@ go y
    S :@ x :@ y :@ z → go (go x :@ go z :@ (go y :@ go z))

    K           → K
    K :@ x      → K :@ go x
    K :@ x :@ y → go x

    J n t                     → J n t
    J 0 _ :@ b                → go b
    J 1 _ :@ b :@ x           → go (b :@ x)
    J 2 _ :@ b :@ x :@ y      → go (b :@ x :@ y)
    J 3 _ :@ b :@ x :@ y :@ z → go (b :@ x :@ y :@ z)
    J n t :@ b                → J n t :@ go b
    J n t :@ b :@ x           → J n t :@ go b :@ go x
    J n t :@ b :@ x :@ y      → J n t :@ go b :@ go x :@ go y
    J n t :@ b :@ x :@ y :@ z → J n t :@ go b :@ go x :@ go y :@ go z

    -- Can I implement the fixedpoint operator using a jet?
    F           → F
    F :@ x      → F :@ go x
    F :@ x :@ y → let xv=go x in go (xv :@ (F :@ xv) :@ go y)

    -- Can I implement naturals using jets?
    N n      → N n
    Inc      → Inc
    Inc :@ x → go x & \case { N n → N (n+1); xv → Inc :@ xv }
    Fol      → Fol
    Fol :@ x → go x & \case { N n → go (fromNat n); xv → Fol :@ xv }

    f :@ x   → go (go f :@ go x)
  where
    go exp = unsafePerformIO $ do
        foo <- evaluate (tshow exp)
        threadDelay 333333
        print exp
        pure (simp exp)

fromNat ∷ Natural → E
fromNat 0 = zer
fromNat 1 = one
fromNat n = inc :@ fromNat (pred n)

toNat ∷ E → Natural
toNat exp =
    case simp (simp exp :@ Inc :@ N 0) of
        N n → n
        e   → error ("bad-nat: " <> show e)

jet n b = J n 0 :@ b

inc, zer, one ∷ E
inc = jet 1 (S :@ (S :@ (K :@ S) :@ K))
zer = S :@ K
one = S :@ K :@ K
