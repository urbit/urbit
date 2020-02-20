{-|
    On 64 bit machines, GHC will always use pointer tagging as long as
    there are less than 8 constructors. So, anything that is frequently
    pattern matched on should have at most 7 branches.

    So, the calling convention this is falling into is:

        - Caller pushes correct number of arguments to stack.
        - If extra arguments, caller calls result with remaining arguments.

    Maybe that should be swapped, a. la zinc (callee decides if it has
    enough arguments).

    But, regardless:

    There are many cases where we invoke a known function with exactly
    the right number of arguments, and it should be possible to determine
    this statically.

    The opposite case is something like: `($0 $1)`. We don't know the
    arity of `$0`.

    However, if I see `(add 9 $2)`, it doesn't make much sense to use
    the argument stack at all.

    And really, in the evaluation of `Exp`s, there are only three cases:

        1. Calls to functions of unknown arity -- use previous approach.
        2. Undersaturated calls to functions of known arity.
        3. Fully saturated calls to functions of known arity.

    There is no such thing as oversaturated calls to functions of known
    arity, since that is simply a fully saturated call followed by another call.

    Let's look at the `acker` example:

        ?:  (iszero $0)
          (inc $1)
        ?:  (iszero $1)
          (RECUR (dec $0) 1)
        (RECUR (dec $0) (RECUR $0 (dec $1)))

    It seems to me that this can be interpreted much more efficiently
    than the approach that's evolving here. Let's work this out by hand
    and see what concepts fall out.

        IF (ISZERO ($0))
        THEN
            INC $0
        ELSE
            IF (ISZERO $1)
            THEN
                (RECUR (DEC $0) 1)
            ELSE
                (RECUR (DEC $0) (RECUR $0 (DEC $1)))

    Notes:

    - IF/ISZERO/etc throw exceptions if not given expected type.
    - In this example, every call is fully saturated. There is no need
      for stack manipulation at all.
    - Except in the recursion case. How should that work?

      - Recursive call is not a primop. Jet will expect to be passed
        arguments.

      - Well, we know that this jet takes two arguments. Can we just
        pass them on the Haskell stack?

        - `runJet2 :: Jet -> Val -> Val -> IO Val`

    That would become:

        IF (ISZERO $0)
        THEN
            INC $0
        ELSE
            IF (ISZERO $1)
            THEN
                (JET2 $self (DEC $0) (VAL 1))
            ELSE
                (JET2 $self (DEC $0) (JET2 $self $0 (DEC $1)))

    This would be pretty fast!

    This example really shows off the idea behind Uruk: That it allows
    the interpreter to work with much higher level concepts.

    What about pattern matching?

    I've been presuming that the result of pattern matching would get
    pushed to the stack. How does this end up working?

    Here's a dumb example:

        |=  n
        ?-  (Dec n)
          (Lef _)  0
          (Rit x)  (Add x n)
        ==

    Right now, it would decompile to something like this:

        CASE (DEC $0)
        LEFT
            (VAL 0)
        RIGT
            (ADD $1 (VAL $0))

    We could keep a per-jet stack (probably just a list?)

    - References to jet parameters would resolve to Haskell arguments
    - Arguments to values intoduced through pattern matching would go
      into a stack.

    We could pre-determine the size of the stack to use a pre-allocated
    mutable array.

        MKFRAME 1
        CASE@0 (DEC $0)
        LEFT
            (VAL 0)
        RIGT
            (ADD $0 (VAL @0))

    I think this is basically register allocation?

    Yes, this will work.

    I don't know how well this will work in general, but let's hack out
    the specific case first.
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

data Clo
    = CJet !Jet
    | CFun !Fun
    | COne !Clo !Val
    | CTwo !Clo !Val !Val
    | CVec !Clo !(Array Val)
  deriving (Eq, Ord, Show)

data Val
    = VUni
    | VTup !Val !Val
    | VSum !Bool !Val
    | VBol !Bol
    | VNat !Nat
    | VClo !Clo !Int
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
            VNat 0 → VSum False VUni
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
    sBuf <- newArray sz VUni
    let sUse = 0
    let sRem = sz
    pure (Stack{..})

growStack ∷ Stack → IO Stack
growStack s@Stack{..} = do
    let !oldSz = sizeofMutableArray sBuf
        !newSz = oldSz * 2
    newBf <- newArray newSz VUni
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

valClo ∷ Val → (Clo, Int)
valClo = \case
    VClo k rem → (k, rem)
    VNat n     → undefined
    VTup x y   → undefined
    VSum t v   → undefined
    VUni       → undefined

callCloVec ∷ Stack → Clo → Int → Array Val → IO (Stack, Val)
callCloVec e k n xs = do
    let !args = sizeofArray xs
    compare n args & \case
        EQ → runClo e (CVec k xs)
        LT → pure (e, VClo (CVec k xs) (n-args))
        GT → (args-n) & \case
            1 → undefined
            2 → undefined
            n → undefined


-- Call Values -----------------------------------------------------------------

call1 ∷ Stack → Val → Val → IO (Stack, Val)
call1 e f x = case f of
    VClo k rem → callClo1 e k rem x
    _          → callClo1 e k rem x where (k, rem) = valClo f

call2 ∷ Stack → Val → Val → Val → IO (Stack, Val)
call2 e f x y = case f of
    VClo k rem → callClo2 e k rem x y
    _          → callClo2 e k rem x y where (k, rem) = valClo f

callVec ∷ Stack → Val → Array Val → IO (Stack, Val)
callVec e f xs = do
    case f of
        VClo k rem → callCloVec e k rem xs
        _          → callCloVec e k rem xs where (k, rem) = valClo f


-- Execute function against populated stack. -----------------------------------

execFun ∷ Fun → Stack → IO (Stack, Val)
execFun !self !initialEnv = go initialEnv (funExpr self)
  where
    evalArgs ∷ Stack → Array Exp → IO (Stack, Array Val)
    evalArgs !env !xs = undefined

    go ∷ Stack → Exp → IO (Stack, Val)
    go !env = \case
        EVal v → do
            pure (env, v)
        EApp f xs  → do
            (env, f)  <- go env f
            (env, xs) <- evalArgs env xs
            callVec env f xs
        ERec → do
            pure (env, VClo (CFun self) (funArgs self))
        ERef ix → do
            (env,) <$> ref env ix
        EIff c t e → go env c >>= \case
            ( env, VBol True  ) → go env t
            ( env, VBol False ) → go env t
            ( _,   _          ) → error "TODO"
        ECas x l r → go env x >>= \case
            ( env, VSum False x ) → do env←push1 env x; go env l
            ( env, VSum True x  ) → do env←push1 env x; go env r
            ( _,   _            ) → error "TODO"
