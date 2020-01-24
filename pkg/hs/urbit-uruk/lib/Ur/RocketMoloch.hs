module Ur.RocketMoloch where

import Ur.Common hiding (flat)
import GHC.Natural

{-
--------------------------------------------------------------------------------

type Nat = Natural


-- Expressions -----------------------------------------------------------------

-- | Syntax tree for expressions
data Exp
    = App Exp Exp
    | Ess
    | Kay
    | Eye
    | Lit Nat
    | Inc
    | Fol
    | Fix
    | Fas
    | Fir
    | Evl
    | Dum
    | Uni
    | Con
    | Car
    | Cdr
    | Lef
    | Rit
    | Cas
    | Add
    | Mul
  deriving (Eq, Ord)
  deriving anyclass (NFData)


-- Printing --------------------------------------------------------------------

instance Show Exp where
  show = go
    where
      appList (App f x) acc = appList f (x:acc)
      appList e         acc = (e:acc)

      go = \case
          App f x → "("   <> intercalate " " (go <$> appList f [x]) <> ")"
          Ess     → "s"
          Kay     → "k"
          Eye     → "i"
          Lit n   → show n
          Inc     → "inc"
          Fol     → "fol"
          Fix     → "fix"
          Fas     → "fas"
          Fir     → "fir"
          Evl     → "evl"
          Dum     → "dum"
          Uni     → "uni"
          Con     → "con"
          Car     → "car"
          Cdr     → "cdr"
          Lef     → "lef"
          Rit     → "rit"
          Cas     → "cas"
          Add     → "add"
          Mul     → "mul"


-- Ig -- Values with no semantic meaning ---------------------------------------

newtype Ig a = Ig a

instance Eq (Ig a) where
  x == y = True

instance Ord (Ig a) where
  compare x y = EQ

instance Show (Ig a)
  where
    show = const "Ig"


-- Interpreter Values ----------------------------------------------------------

data Val
    = VFun Exp [Val]
    | VFix Exp [Val]
    | VNat Nat
    | VJet Val (Ig (Val → IO Val))
    | VUni
    | VCon Val Val
    | VLef Val
    | VRit Val
  deriving (Eq, Ord, Show)


-- Interpreter -----------------------------------------------------------------

eval :: E -> V
eval = \case
    App f x → c (eval f) (eval x)
    Ess     → fn3 Ess \x y z → x `c` z `c` (y `c` z)
    Fix     → fn1 Fix fix
    Kay     → fn2 Kat const
    Eye     → fn1 Eye id
    Lit n   → VNat n
    Inc     → fn1 Inc inc
    Fol     → fn1 Fol fol
    Fas     → fn1 Fas jet
    Fir     → fn2 Fir run
    Evl     → fn1 Evl evl
    Dum     → fn1 Dum dum
    Uni     → VUni
    Con     → fn2 x y
    Car     → fn1 car
    Cdr     → fn1 cdr
    Lef     → fn1 lef
    Rit     → fn1 rit
    Cas     → fn3 cas
    Add     → fn1 add
    Mul     → fn1 mul

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
    inc x     = error "bad-inc"

    fol (N n) = trace "FOL" $ lNat n
    fol v     = error "bad-fol"
      where
        lNat 0 = fn2 (S :@ K) \i z → z
        lNat n = lSucc `c` lNat (pred n)
          where lSucc = fn3 (S :@ (S :@ (K :@ S) :@ K))
                            (\n i z → i `c` (n `c` i `c` z))

    fir (B (F _ f)) x = f x
    fir jf          x = error "bad-fir"

    dum = N . encodeExp . valExp

    evl (N (decodeExp -> Jet x)) = eval x
    evl v                        = error "bad-evl"

    add (VCon (VNat x) (VNat y)) = VNat (x+y)
    add v                        = error "bad-add"

    mul (VCon (VNat x) (VNat y)) = VNat (x*y)
    mul v                        = error "bad-mul"

decodeExp :: Nat -> Maybe Exp
decodeExp = undefined

encodeExp :: Exp -> Nat
encodeExp = undefined

valExp :: Val -> Exp
valExp = undefined

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
-}
