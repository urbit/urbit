{-|
    On 64 bit machines, GHC will always use pointer tagging as long as
    there are less than 8 constructors. So, anything that is frequently
    pattern matched on should have at most 7 branches.

    ----------------------------------------

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

    ----------------------------------------

    Alright, what's left to do here?

    Let's break down this thing:

        IF (ISZERO $0)
        THEN
            INC $0
        ELSE
            IF (ISZERO $1)
            THEN
                (JET2 $self (DEC $0) (VAL 1))
            ELSE
                (JET2 $self (DEC $0) (JET2 $self $0 (DEC $1)))

    What are the types of expressions?

-}

module Uruk.Fast where

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

data Pri
    = S
    | K
    | J
    | D
  deriving (Eq, Ord, Show)

data Fun
    = FClo Fun (Array Val)
    | FJet Jet
    | FVal Val
    | FPri Pri
  deriving (Eq, Ord, Show)

data Val
    = VUni
    | VCon Val Val
    | VLef Val
    | VRit Val
    | VNat Nat
    | VBol Bool
    | VFun Int Fun
  deriving (Eq, Ord, Show)

data Jet = Jet
    { jArgs ∷ Int
    , jName ∷ Val
    , jBody ∷ Val
    , jFast ∷ Exp
    , jRegs ∷ Int -- Number of registers needed.
    }
  deriving (Eq, Ord, Show)

data Exp
    = VAL Val                  --  Constant Value
    | REF Int                  --  Stack Referene
    | REC (Array Exp)          --  Recursive Call
    | SLF                      --  Self Reference
    | IFF Exp Exp Exp          --  If-Then-Else
    | CAS Int Exp Exp Exp      --  Pattern Match

    | SEQ Exp Exp              --  Evaluate head, return tail
    | DED Exp                  --  Evaluate argument, then crash.

    | INC Exp                  --  Increment
    | DEC Exp                  --  Decrement
    | FEC Exp                  --  Fast decrement
    | MUL Exp Exp              --  Multiply
    | SUB Exp Exp              --  Subtract
    | ZER Exp                  --  Is Zero?
    | EQL Exp Exp              --  Atom equality.

    | UNI                      --  Unit
    | CON Exp Exp              --  Cons
    | CAR Exp                  --  Head
    | CDR Exp                  --  Tail
    | LEF Exp                  --  Left Constructor
    | RIT Exp                  --  Right Constructor

    | JET Jet (Array Exp)      --  Fully saturated call
    | CLO Fun Int (Array Exp)  --  Undersaturated call
    | CAL Exp (Array Exp)      --  Call of unknown saturation
  deriving (Eq, Ord, Show)


--------------------------------------------------------------------------------

data TypeError = TypeError
  deriving (Eq, Ord, Show, Exception)

data Crash = Crash Val
  deriving (Eq, Ord, Show, Exception)

type IOArray = MutableArray RealWorld

emptyRegisterSet ∷ IOArray Val
emptyRegisterSet = unsafePerformIO (newArray 0 VUni)


--------------------------------------------------------------------------------

execJet ∷ Jet → Array Val → IO Val
execJet j xs = do
    regs <- case jRegs j of
                0 → pure emptyRegisterSet
                n → newArray n VUni

    handle (\TypeError → runSlow) $
        execJetBody j xs regs

  where
    runSlow = do
        error "TODO"

execFun ∷ Fun → IO Val
execFun f = do
    error "TODO"

execJetBody ∷ Jet → Array Val → IOArray Val → IO Val
execJetBody j xs regs = go (jFast j)
  where
    setReg ∷ Int → Val → IO ()
    setReg i x = writeArray regs i x

    go ∷ Exp → IO Val
    go = \case
        VAL v  → pure v
        REF i  → pure $ indexArray xs i
        REC xs → execJet j =<< traverse go xs
        SLF    → pure $ VFun (jArgs j) $ FJet j

        IFF c t e → do
            go c >>= \case
                VBol True  → go t
                VBol False → go e
                _          → throwIO TypeError

        CAS i x l r → do
            go x >>= \case
                VLef lv → do
                   setReg i lv
                   go l
                VRit rv → do
                   setReg i rv
                   go r

        SEQ x y → go x >> go y
        DED x   → throwIO . Crash =<< go x

        INC x → go x >>= \case VNat x → pure $ VNat (x+1)
                               _      → throwIO TypeError

        DEC x → go x >>= \case VNat 0 → pure $ VLef VUni
                               VNat n → pure $ VRit (VNat (n-1))
                               _      → throwIO TypeError

        FEC x → go x >>= \case VNat 0 → pure $ VNat 0
                               VNat n → pure $ VNat (n-1)
                               _      → throwIO TypeError

        MUL x y → do
            xv <- go x
            yv <- go y
            case (xv, yv) of
                (VNat x, VNat y) → pure (VNat (x*y))
                (_,      _     ) → throwIO TypeError

        SUB x y → do
            xv <- go x
            yv <- go y
            case (xv, yv) of
                (VNat x, VNat y) | y>x → pure (VLef VUni)
                (VNat x, VNat y)       → pure (VRit (VNat (x-y)))
                (_,      _     )       → throwIO TypeError

        ZER x → go x >>= \case VNat 0 → pure (VBol True)
                               VNat n → pure (VBol False)
                               _      → throwIO TypeError

        EQL x y → do
            xv <- go x
            yv <- go y
            case (xv, yv) of
                (VNat x, VNat y) → pure (VBol (x == y))
                (_,      _     ) → throwIO TypeError

        UNI → do
            pure VUni

        CON x y → do
            xv <- go x
            yv <- go y
            pure (VCon xv yv)

        CAR x → go x >>= \case VCon x _ → pure x
                               _        → throwIO TypeError

        CDR x → go x >>= \case VCon _ y → pure y
                               _        → throwIO TypeError

        LEF x → VLef <$> go x
        RIT x → VRit <$> go x

        JET j xs → do
            xs <- traverse go xs
            execJet j xs

        CLO f i xs → do
            clo <- FClo f <$> traverse go xs
            pure (VFun i clo)

        CAL f xs → do
            fv <- go f
            xs <- traverse go xs
            execFun $ FClo (FVal fv) xs
