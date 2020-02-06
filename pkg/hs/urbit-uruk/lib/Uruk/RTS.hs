{-|
    On 64 bit machines, GHC will always use pointer tagging as long as
    there are less than 8 constructors. So, anything that is frequently
    pattern matched on should have at most 7 branches.
-}

module Uruk.RTS where

import ClassyPrelude    hiding (evaluate, try)
import System.IO.Unsafe
import Data.Primitive.Array

import Control.Arrow    ((>>>))
import Data.Function    ((&))
import Numeric.Natural  (Natural)
import Numeric.Positive (Positive)
import Prelude          ((!!))
import Uruk.JetDemo     (Ur, UrPoly(Fast))

import qualified Uruk.JetComp as Comp
import qualified Uruk.JetDemo as Ur

--------------------------------------------------------------------------------

type Nat = Natural
type Bol = Bool
type Pos = Positive

--------------------------------------------------------------------------------

data Exp
    = EVal !Val
    | EApp !Exp !(Array Exp)
    | ERec
    | ERef !Int
    | EIff !Exp !Exp !Exp
    | ECas !Exp !Exp !Exp
  deriving (Eq, Ord, Show)

-- | Rare data. Performance less important.
data Wak
    = WakNat !Nat !Val
    | WakBol !Bol !Val
    | WakUni
  deriving (Eq, Ord, Show)

data Clo
    = CJet !Jet
    | CFun !Fun
    | COne !Clo !Val
    | CTwo !Clo !Val !Val
    | CVec !Clo !(Array Val)
  deriving (Eq, Ord, Show)

data Val
    = VBol !Bol
    | VNat !Nat
    | VTup !Val !Val
    | VSum !Bool !Val
    | VClo !Clo !Int
    | VWak !Wak
  deriving (Eq, Ord, Show)

data Fun = Fun
    { funExpr ∷ !Exp
    , funArgs ∷ !Int
    , funName ∷ !Val
    , funValu ∷ !Val
    }
  deriving (Eq, Ord, Show)

data Jet
    = S | Sn | K | D | B | Bn | C | Cn | I
    | Seq | Yet | Fix | Iff | Ded
    | Pak | Zer | Eql | Add | Inc | Dec | Fec | Mul | Sub
    | Uni
    | Cas
    | Con | Car | Cdr
  deriving stock (Eq, Ord, Show)

jam ∷ Val → Val
jam = error "TODO"

unit ∷ Val
unit = VWak WakUni

execJet ∷ Stack → Jet → Val
execJet !env = undefined {-
  where
    v n = env !! n

    -- TODO Jet invariants broken, fallback to raw jet implementation.
    fallback = error "TODO: fallback"

    go = \case

        S   → case (v 2, v 1, v 0) of (x,y,z) → call2 x z (call1 y z)
        K   → v 1
        I   → v 0
        D   → jam (v 0)
        Sn  → error "TODO"
        B   → case (v 2, v 1, v 0) of (f,g,x) → call1 f (call1 g x)
        Bn  → error "TODO"
        C   → case (v 2, v 1, v 0) of (f,g,x) → call2 f x g
        Cn  → error "TODO"
        Seq → v 0
        Yet → error "TODO"
        Fix → error "TODO"
        Iff → v 2 & \case
            VBol True  → v 1
            VBol False → v 0
            _          → fallback
        Ded → error "ded"
        Pak → fallback
        Zer → v 0 & \case
            VNat 0 → VBol True
            VNat n → VBol False
            _      → fallback
        Eql → (v 1, v 0) & \case
            (VNat x, VNat y) → VBol (x == y)
            (_,      _     ) → fallback
        Add → (v 1, v 0) & \case
            (VNat x, VNat y) → VNat (x+y)
            (_,      _     ) → fallback
        Inc → v 0 & \case
            VNat n → VNat (n+1)
            _      → fallback
        Dec → v 0 & \case
            VNat 0 → VSum False unit
            VNat n → VNat (n-1)
            _      → fallback
        Fec → v 0 & \case
            VNat 0 → VNat 0
            VNat n → VNat (n-1)
            _      → fallback
        Mul → (v 1, v 0) & \case
            (VNat x, VNat y) → VNat (x*y)
            (_,      _     ) → fallback
        Sub → (v 1, v 0) & \case
            (VNat x, VNat y) → VNat (x*y)
            (_,      _     ) → fallback
        Uni → fallback
        Cas → fallback
        Con → error "TODO"
        Car → v 0 & \case
            VTup x _ → x
            _        → fallback
        Cdr → v 0 & \case
            VTup _ y → y
            _        → fallback
-}


-- Argument Stack --------------------------------------------------------------

data Stack = Stack
    { sBuf ∷ !(MutableArray RealWorld Val)
    , sUse ∷ !Int
    , sRem ∷ !Int
    }

newStack ∷ Int → IO Stack
newStack sz = do
    sBuf <- newArray sz unit
    let sUse = 0
    let sRem = sz
    pure (Stack{..})

growStack ∷ Stack → IO Stack
growStack s@Stack{..} = do
    let !oldSz = sizeofMutableArray sBuf
        !newSz = oldSz * 2
    newBf <- newArray newSz unit
    copyMutableArray newBf 0 sBuf 0 sUse
    pure (Stack newBf sUse (sRem + (newSz - oldSz)))

push1 ∷ Stack → Val → IO Stack
push1 s@Stack{..} x | sRem < 1 = do { s ← growStack s; push1 s x }
push1 s@Stack{..} x            = do
    writeArray sBuf sUse x
    pure (Stack sBuf (sUse+1) (sRem-1))

push2 ∷ Stack → Val → Val → IO Stack
push2 s@Stack{..} x y | sRem < 2 = do { s ← growStack s; push2 s x y }
push2 s@Stack{..} x y            = do
    writeArray sBuf sUse x
    writeArray sBuf (sUse+1) x
    pure (Stack sBuf (sUse+2) (sRem-2))

pushVec ∷ Stack → Array Val → IO Stack
pushVec s@Stack{..} xs | sRem < sizeofArray xs = do
    s ← growStack s
    pushVec s xs
pushVec s@Stack{..} xs                    = do
    copyArray dst dstOff src srcOff num
    pure (Stack sBuf (sUse+num) (sRem-num))
  where
    dst    = sBuf
    dstOff = sUse
    num    = sizeofArray xs
    src    = xs
    srcOff = 0

-- | TODO Use unboxed tuples and sums?
pushClo ∷ Stack → Clo → IO (Stack, Either Jet Fun)
pushClo s = \case
    CJet j   → pure (s, Left j)
    CFun f   → pure (s, Right f)
    COne k x → do
        (s,r) ← pushClo s k
        s     ← push1 s x
        pure (s,r)
    CTwo k x y → do
        (s,r) ← pushClo s k
        s     ← push2 s x y
        pure (s,r)
    CVec k xs → do
        (s,r) ← pushClo s k
        s     ← pushVec s (undefined xs)
        pure (s, r)

ref ∷ Stack → Int → IO Val
ref Stack{..} ix = readArray sBuf ix


--------------------------------------------------------------------------------

-- | Execute a closure whos arguments are saturated
runClo ∷ Stack → Clo → IO (Stack, Val)
runClo env clo = do
    (env, oper) <- pushClo env clo
    case oper of
        Left jet → undefined
        Right fn → undefined

callClo1 ∷ Stack → Clo → Int → Val → IO (Stack, Val)
callClo1 env clo 1 x = runClo env (COne clo x)
callClo1 env clo n x = pure (env, VClo (COne clo x) (n-1))

callClo2 ∷ Stack → Clo → Int → Val → Val → IO (Stack, Val)
callClo2 e k 1 x y = runClo e (CTwo k x y)
callClo2 e k 2 x y = do (e,acc) <- runClo e (COne k x); call1 e acc y
callClo2 e k n x y = pure (e, VClo (CTwo k x y) (n-2))

callCloVec ∷ Stack → Clo → Int → Array Val → IO (Stack, Val)
callCloVec e k n xs = undefined
            {-
            compare args rem & \case
                EQ → runClo env (CVec clo xs)
                LT → pure (env, VClo (CVec clo xs) (rem - args))
                GT → undefined -- call with some args, call result with rest.
            -}

call1 ∷ Stack → Val → Val → IO (Stack, Val)
call1 env f x = case f of
    VClo clo rem → callClo1 env clo rem x
    _            → error "TODO"

call2 ∷ Stack → Val → Val → Val → IO (Stack, Val)
call2 env f x y = case f of
    VClo clo rem → callClo2 env clo rem x y
    _            → error "TODO"

callVec ∷ Stack → Val → Array Val → IO (Stack, Val)
callVec env f xs = do
    case f of
        VClo clo rem → callCloVec env clo rem xs
        _            → error "TODO"

execFun ∷ Fun → Stack → IO (Stack, Val)
execFun !self !initialEnv = go initialEnv (funExpr self)
  where
    evalArgs ∷ Stack → Array Exp → IO (Stack, Array Val)
    evalArgs env xs = undefined

    go ∷ Stack → Exp → IO (Stack, Val)
    go !env = \case
        EVal v     → pure (env, v)
        EApp f xs  → do (env, f)  <- go env f
                        (env, xs) <- evalArgs env xs
                        callVec env f xs
        ERec       → pure (env, VClo (CFun self) (funArgs self))
        ERef ix    → (env,) <$> ref env ix
        EIff c t e → go env c >>= \case
                        ( env, VBol True  ) → go env t
                        ( env, VBol False ) → go env t
                        ( _,   _          ) → error "TODO"
        ECas x l r → go env x >>= \case
                         ( env, VSum False x ) → do env←push1 env x; go env l
                         ( env, VSum True x  ) → do env←push1 env x; go env r
                         ( _,   _            ) → error "TODO"
