module Ur.Max where

import Uruk.Exp
import Ur.Common  hiding (A, C, L, R, flat)

import Control.Lens (from, view)
import Data.Flat    (flat, Flat)
import GHC.Natural  (Natural)
import Noun         (atomBytes)


-- Switch to List-Based Representation -----------------------------------------

type Nat = Natural

data Com = S | K | I
         | C | K | B
         | Sn Nat
         | Bn Nat
         | Cn Nat

data P = S | K | I                --  SKI
       | Jet Natural Text         --  Jets
       | Fix                      --  Fixed-Point
       | N Natural | Inc | Fol    --  Nats
       | Dump                     --  Serialize expression.
       | Unit                     --  Unit
       | Con | Car | Cdr          --  Pair
       | Lef | Rit | Cas          --  Sum
       | Add | Mul | Dec          --  Natural Algebra

  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (NFData, Flat)

--  The arity of each built-in combinator.
tagPrim ∷ P → V P
tagPrim prim = V prim (arity prim) []
  where
    arity = \case
        S       → 3
        K       → 2
        I       → 1
        Jet _ _ → 1    --  Jet oper fires when body is supplied then
                       --  fires again when args are supplied.
        Fix     → 2
        N n     → 1    --  function-wrapped
        Inc     → 1
        Fol     → 1
        Dump    → 1
        Unit    → 1    --  function-wrapped
        Con     → 3
        Car     → 1
        Cdr     → 1
        Lef     → 2    --  function-wrapped
        Rit     → 2    --  function-wrapped
        Cas     → 3

        Dec     → 1
        Add     → 2
        Mul     → 2


--  This function will crash if combinators are simplified with the
--  wrong number of arguments.
simplifyPrim ∷ P → [V P] → X P
simplifyPrim = curry go
  where
    go ∷ (P, [V P]) → X P
    go ( S,       [z,y,x]  ) = C x :@ C z :@ (C y :@ C z)
    go ( K,       [y,x]    ) = C x
    go ( I,       [x]      ) = C x
    go ( Dump,    [x]      ) = C (tagPrim (dump x))
    go ( Unit,    [_]      ) = C iVal
    go ( Lef,     [_,x]    ) = C x
    go ( Rit,     [_,x]    ) = C x
    go ( Cas,     [x,r,l]  ) = C x :@ C unit :@ C l :@ C r
    go ( Fix,     [x,f]    ) = C f :@ C (V Fix 1 [f]) :@ C x
    go ( N n,     [_]      ) = C $ foldNat n
    go ( Inc,     [x]      ) = C $ tagPrim $ N $ succ $ getNat x
    go ( Fol,     [x]      ) = C $ foldNat $ getNat x
    go ( Jet c n, [b]      ) = jetMatch c n b
    go ( Jet _ _, x:xs     ) = jetFire x xs
    go ( Con,     [f,y,x]  ) = C f :@ C x :@ C y
    go ( Car,     [p]      ) = doCar p
    go ( Cdr,     [p]      ) = doCdr p

    go ( Dec,     [x]      ) = doDec x
    go ( Mul,     [y,x]    ) = doMul x y
    go ( Add,     [y,x]    ) = doAdd x y

    go ( p,       ar       ) = error ("prim-no-simplify: " <> show (p,ar))

    doCar (V Con 1 [_,h]) = C h
    -- ar p               = C uCar :@ C p
    doCar p               = C p :@ carBody

    doCdr (V Con 1 [t,_]) = C t
    -- dr p               = C uCdr :@ C p
    doCdr p               = C p :@ cdrBody

    doDec (V (N 0) 1 _) = prep (C Lef :@ C Unit)
    doDec (V (N n) 1 _) = prep (C Rit :@ C (N $ pred n))
    doDec x             = C uDec :@ C x

    doAdd (V (N x) 1 []) (V (N y) 1 []) = C $ V (N (x+y)) 1 []
    doAdd x              y              = C uAdd :@ C x :@ C y

    doMul (V (N x) 1 []) (V (N y) 1 []) = C $ V (N (x*y)) 1 []
    doMul x              y              = C uMul :@ C x :@ C y

    carBody = prep (C S :@ C I :@ (C K :@ C K))
    cdrBody = prep (C S :@ C I :@ (C K :@ (C S :@ C K)))

    iVal = tagPrim I
    unit = tagPrim Unit

    getNat ∷ V P → Nat
    getNat (V (N n) _ _) = n
    getNat (V _     _ _) = error "bad-nat"

    -- TODO Actually do jet matching.
    jetMatch ∷ Nat → Text → V P → X P
    jetMatch c n b = C (V (Jet c n) c [b])

    jetFire ∷ V P → [V P] → X P
    jetFire f []     = C f
    jetFire x [f]    = C f :@ C x
    jetFire l (x:xs) = jetFire x xs :@ C l

    dump = N . view (from atomBytes) . flat . cDump

{-
    dec = \x -> Fol x (\x -> Cas x (\l -> Rit Zero) (\x -> Rit (Inc x))) (Lef Unit)
    mul x y i z = Fol x (Fol y i) z
    add x y i z = Fol y i (Fol x i z)

    doDec (V (N 0) 1 _) = prep (C Lef :@ C Unit)
    doDec (V (N n) 1 _) = prep (C Rit :@ C (N $ pred n))
-}
uDec, uAdd, uMul ∷ V P
uDec = eval (C (Jet 1 "dec") :@ b)
  where
    b = s :@ (s :@ fol :@ (k:@ (s :@ (s:@cas:@(k:@(k:@(rit:@zer))))
                                  :@ (k:@(s:@(k:@rit):@inc)))))
          :@ (k:@(lef:@unit))


uAdd = eval (C (Jet 2 "add") :@ b)
  where
    b = s :@ (k:@(s:@(s:@(k:@(s:@(k:@s):@(s:@(k:@(s:@(k:@s):@k))))):@fol)))
          :@ (s:@(k:@k):@fol)

uMul = eval (C (Jet 2 "mul") :@ b)
  where
    b = s:@(s:@(k:@(s:@(k:@(s:@(k:@(s:@(k:@s):@k)):@s)):@k)):@fol):@(k:@fol)

s,k,i,fol ∷ E P
(s,k,i,fol) = (C S, C K, C I, C Fol)

unit, lef, rit :: E P
unit = C Unit
lef  = C Lef
rit  = C Rit
cas  = C Cas
inc  = C Inc

eval ∷ E P → V P
eval = reEval . prep

prep ∷ E P → X P
prep = cPrep tagPrim

reEval ∷ X P → V P
reEval = cEval simplifyPrim

zer, one ∷ E P
zer = (C S :@ C K)
one = C I

uInc ∷ V P
uInc = eval $ C S :@ (C S :@ (C K :@ C S) :@ C K)

foldNat ∷ Nat → V P
foldNat = reEval . go
  where go 0 = prep zer
        go 1 = prep one
        go n = C uInc :@ C (foldNat $ pred n)
