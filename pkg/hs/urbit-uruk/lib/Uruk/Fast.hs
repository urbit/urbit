{-# OPTIONS_GHC -funbox-strict-fields #-}

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

import ClassyPrelude    hiding (evaluate, try, fromList)
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

data Pri = S | K | J | D
  deriving (Eq, Ord, Show)

data Fun
    = FClo !Int !Fun !(Array Val)
    | FJet !Int !Jet
    | FPri !Int !Pri
  deriving (Eq, Ord, Show)

data Val
    = VUni
    | VCon !Val !Val
    | VLef !Val
    | VRit !Val
    | VNat !Nat
    | VBol !Bool
    | VFun !Fun
  deriving (Eq, Ord, Show)

data Jet = Jet
    { jArgs ∷ !Int
    , jName ∷ Val
    , jBody ∷ Val
    , jFast ∷ !Exp
    , jRegs ∷ !Int -- Number of registers needed.
    }
  deriving (Eq, Ord, Show)

data Exp
    = VAL !Val                 --  Constant Value
    | REF !Int                 --  Stack Reference
    | REC2 !Exp !Exp           --  Recursive Call (two args)
    | RECN !(Array Exp)        --  Recursive Call (arbitrary args)
    | SLF                      --  Self Reference
    | IFF !Exp !Exp !Exp       --  If-Then-Else
    | CAS !Int !Exp !Exp !Exp  --  Pattern Match

    | SEQ !Exp !Exp            --  Evaluate head, return tail
    | DED !Exp                 --  Evaluate argument, then crash.

    | INC !Exp                 --  Increment
    | DEC !Exp                 --  Decrement
    | FEC !Exp                 --  Fast decrement
    | ADD !Exp !Exp            --  Add
    | MUL !Exp !Exp            --  Multiply
    | SUB !Exp !Exp            --  Subtract
    | ZER !Exp                 --  Is Zero?
    | EQL !Exp !Exp            --  Atom equality.

    | UNI                      --  Unit
    | CON !Exp !Exp            --  Cons
    | CAR !Exp                 --  Head
    | CDR !Exp                 --  Tail
    | LEF !Exp                 --  Left Constructor
    | RIT !Exp                 --  Right Constructor

    | JETN !Jet !(Array Exp)   --  Fully saturated call
    | JET2 !Jet !Exp !Exp      --  Fully saturated call
    | CLON !Fun !(Array Exp)    --  Undersaturated call
    | CALN !Exp !(Array Exp)    --  Call of unknown saturation
  deriving (Eq, Ord, Show)


--------------------------------------------------------------------------------

valArity ∷ Val → Int
valArity = error "TODO"

funArgs ∷ Fun → Int
{-# INLINE funArgs #-}
funArgs = \case
    FClo i _ _ → i
    FJet i _   → i
    FPri i _   → i

--------------------------------------------------------------------------------

data TypeError = TypeError Text
  deriving (Eq, Ord, Show, Exception)

data Crash = Crash Val
  deriving (Eq, Ord, Show, Exception)

data BadRef = BadRef Jet Int
  deriving (Eq, Ord, Show, Exception)

type IOArray = MutableArray RealWorld

emptyRegisterSet ∷ IOArray Val
emptyRegisterSet = unsafePerformIO (newArray 0 VUni)


--------------------------------------------------------------------------------

execFun ∷ Fun → IO Val
{-# INLINE execFun #-}
execFun !f =
    compare (funArgs f) 0 & \case
        GT → pure (VFun f)
        EQ → execFunFull f
        LT → error "TODO"

uniJet ∷ Jet
uniJet = error "TODO"

conJet ∷ Val → Val → Jet
conJet = error "TODO"

lefJet ∷ Val → Jet
lefJet = error "TODO"

ritJet ∷ Val → Jet
ritJet = error "TODO"

natJet ∷ Nat → Jet
natJet = error "TODO"

bolJet ∷ Bol → Jet
bolJet = error "TODO"

valFun ∷ Val → Fun
{-# INLINE valFun #-}
valFun = \case
    VUni     → FJet 1 uniJet
    VCon h t → FJet 2 (conJet h t)
    VLef l   → FJet 1 (lefJet l)
    VRit r   → FJet 1 (ritJet r)
    VNat n   → FJet 2 (natJet n)
    VBol b   → FJet 2 (bolJet b)
    VFun f   → f

jam ∷ Val → Val
jam = error "TODO"

execPrim ∷ Pri → Array Val → IO Val
{-# INLINE execPrim #-}
execPrim !p !xs =
    p & \case
        S → error "TODO"
        K → pure (v 0)
        D → pure $ jam $ v 0
        J → error "TODO"
  where
    v = indexArray xs

execFunFull ∷ Fun → IO Val
{-# INLINE execFunFull #-}
execFunFull = go []
  where
    go ∷ [Array Val] → Fun → IO Val
    go !acc = \case
        FPri _ p    → execPrim p (mconcat acc)
        FClo _ f xs → go (xs:acc) f
        FJet _ j    → execJet j (mconcat acc)

execJet ∷ Jet → Array Val → IO Val
{-# INLINE execJet #-}
execJet !j !xs = do
    regs <- case jRegs j of
                0 → pure emptyRegisterSet
                n → newArray n VUni

    handle (\(TypeError x) → runSlow x) $
        execJetBody j xs regs

  where
    runSlow why = do
        putStrLn ("FALLBACK: " <> why)
        execFunFull $ FClo 0 (valFun $ jBody j) xs

execJet2 ∷ Jet → Val → Val → IO Val
{-# INLINE execJet2 #-}
execJet2 !j !x !y = do
    handle (\(TypeError x) → runSlow x) $
        execJetBody2 j x y

  where
    runSlow why = do
        putStrLn ("FALLBACK: " <> why)
        execFunFull $ FClo 0 (valFun $ jBody j) (fromList [x,y])


--------------------------------------------------------------------------------

jetN :: (Exp -> IO Val) -> Jet -> Array Exp -> IO Val
{-# INLINE jetN #-}
jetN go j xs = do
  xs <- traverse go xs
  execJet j xs

jet2 :: (Exp -> IO Val) -> Jet -> Exp -> Exp -> IO Val
{-# INLINE jet2 #-}
jet2 go j x y = do
  x <- go x
  y <- go y
  execJet2 j x y

iff :: (Exp -> IO Val) -> Exp -> Exp -> Exp -> IO Val
{-# INLINE iff #-}
iff go c t e = go c >>= \case
  VBol True  -> go t
  VBol False -> go e
  _          -> throwIO (TypeError "iff-not-bol")

cas
  :: (Exp -> IO Val)
  -> (Int -> Val -> IO ())
  -> Int
  -> Exp
  -> Exp
  -> Exp
  -> IO Val
{-# INLINE cas #-}
cas go setReg i x l r = go x >>= \case
  VLef lv -> setReg i lv >> go l
  VRit rv -> setReg i rv >> go r
  _       -> throwIO (TypeError "cas-no-sum")

inc ∷ (Exp -> IO Val) -> Exp -> IO Val
{-# INLINE inc #-}
inc go x = go x >>= \case
    VNat x → pure $ VNat (x+1)
    _      → throwIO (TypeError "inc-not-nat")

dec ∷ (Exp -> IO Val) -> Exp -> IO Val
{-# INLINE dec #-}
dec go x = go x >>= \case
    VNat 0 → pure $ VLef VUni
    VNat n → pure $ VRit (VNat (n-1))
    _      → throwIO (TypeError "dec-not-nat")

fec ∷ (Exp -> IO Val) -> Exp -> IO Val
{-# INLINE fec #-}
fec go x = go x >>= \case
    VNat 0 → pure $ VNat 0
    VNat n → pure $ VNat (n-1)
    _      → throwIO (TypeError "fec-not-nat")

--------------------------------------------------------------------------------


execJetBody ∷ Jet → Array Val → IOArray Val → IO Val
{-# INLINE execJetBody #-}
execJetBody !j !xs !regs = go (jFast j)
  where
    setReg ∷ Int → Val → IO ()
    setReg i x = writeArray regs i x

    go ∷ Exp → IO Val
    go = \case
        VAL v       → pure v
        REF i       → pure $ indexArray xs i
        REC2 x y    → join (execJet2 j <$> go x <*> go y)
        RECN xs     → join (execJet j <$> traverse go xs)
        SLF         → pure (jetVal j)
        IFF c t e   → iff go c t e
        CAS i x l r → cas go setReg i x l r
        SEQ x y     → go x >> go y
        DED x       → throwIO . Crash =<< go x
        INC x       → inc go x
        DEC x       → dec go x
        FEC x       → fec go x
        LEF x       → VLef <$> go x
        RIT x       → VRit <$> go x
        JET2 j x y  → jet2 go j x y
        JETN j xs   → jetN go j xs
        UNI         → pure VUni

        ADD x y → (,) <$> go x <*> go y >>= \case
            (VNat x, VNat y) → pure (VNat (x+y))
            (_,      _     ) → throwIO (TypeError "add-not-nat")

        MUL x y → (,) <$> go x <*> go y >>= \case
            (VNat x, VNat y) → pure (VNat (x*y))
            (_,      _     ) → throwIO (TypeError "mul-not-nat")

        SUB x y → (,) <$> go x <*> go y >>= \case
            (VNat x, VNat y) | y>x → pure (VLef VUni)
            (VNat x, VNat y)       → pure (VRit (VNat (x-y)))
            (_,      _     )       → throwIO (TypeError "sub-not-nat")

        ZER x → go x >>= \case
            VNat 0 → pure (VBol True)
            VNat n → pure (VBol False)
            xv     → throwIO (TypeError ("zer-not-nat: " <> tshow xv))

        EQL x y → (,) <$> go x <*> go y >>= \case
            (VNat x, VNat y) → pure (VBol (x == y))
            (_,      _     ) → throwIO (TypeError "eql-not-nat")

        CON x y → VCon <$> go x <*> go y

        CAR x → go x >>= \case
            VCon x _ → pure x
            _        → throwIO (TypeError "car-not-con")

        CDR x → go x >>= \case
            VCon _ y → pure y
            _        → throwIO (TypeError "cdr-not-con")

        CLON f xs → do
            let args    = funArgs f
            let remArgs = args - sizeofArray xs
            clo <- FClo remArgs f <$> traverse go xs
            pure (VFun clo)

        CALN f xs → do
            fv <- go f
            xs <- traverse go xs
            let args    = valArity fv
            let remArgs = args - sizeofArray xs
            execFun $ FClo remArgs (valFun fv) xs


--------------------------------------------------------------------------------

jetVal ∷ Jet → Val
jetVal j = VFun $ FJet (jArgs j) j

execJetBody2 ∷ Jet → Val → Val → IO Val
{-# INLINE execJetBody2 #-}
execJetBody2 !j !x !y = go (jFast j)
  where
    go ∷ Exp → IO Val
    go = \case
        VAL v       → pure v
        REF 0       → pure x
        REF 1       → pure y
        REF n       → throwIO (BadRef j n)
        REC2 x y    → join (execJet2 j <$> go x <*> go y)
        RECN xs     → join (execJet j <$> traverse go xs)
        SLF         → pure (jetVal j)
        IFF c t e   → iff go c t e
        CAS i x l r → cas go (error "TODO") i x l r
        SEQ x y     → go x >> go y
        DED x       → throwIO . Crash =<< go x
        INC x       → inc go x
        DEC x       → dec go x
        FEC x       → fec go x
        JETN j xs   → jetN go j xs
        JET2 j x y  → jet2 go j x y
        UNI         → pure VUni

        ADD x y → (,) <$> go x <*> go y >>= \case
            (VNat x, VNat y) → pure (VNat (x+y))
            (_,      _     ) → throwIO (TypeError "add-not-nat")

        MUL x y → (,) <$> go x <*> go y >>= \case
            (VNat x, VNat y) → pure (VNat (x*y))
            (_,      _     ) → throwIO (TypeError "mul-not-nat")

        SUB x y → (,) <$> go x <*> go y >>= \case
            (VNat x, VNat y) | y>x → pure (VLef VUni)
            (VNat x, VNat y)       → pure (VRit (VNat (x-y)))
            (_,      _     )       → throwIO (TypeError "sub-not-nat")

        ZER x → go x >>= \case
            VNat 0 → pure (VBol True)
            VNat n → pure (VBol False)
            xv     → throwIO (TypeError ("zer-not-nat: " <> tshow xv))

        EQL x y → (,) <$> go x <*> go y >>= \case
            (VNat x, VNat y) → pure (VBol (x == y))
            (_,      _     ) → throwIO (TypeError "eql-not-nat")


        CON x y → VCon <$> go x <*> go y

        CAR x → go x >>= \case
            VCon x _ → pure x
            _        → throwIO (TypeError "car-not-con")

        CDR x → go x >>= \case
            VCon _ y → pure y
            _        → throwIO (TypeError "cdr-not-con")

        LEF x → VLef <$> go x
        RIT x → VRit <$> go x

        CLON f xs → do
            let args    = funArgs f
            let remArgs = args - sizeofArray xs
            clo <- FClo remArgs f <$> traverse go xs
            pure (VFun clo)

        CALN f xs → do
            fv <- go f
            xs <- traverse go xs
            let args    = valArity fv
            let remArgs = args - sizeofArray xs
            execFun $ FClo remArgs (valFun fv) xs


-- Trivial Example -------------------------------------------------------------

exampleJet ∷ Jet
exampleJet = Jet
    { jArgs = 2
    , jName = error "HACK"
    , jBody = error "HACK"
    , jFast = exp
    , jRegs = 0
    }
  where
    exp = ADD (REF 0) (ADD (REF 1) (REF 1))

example ∷ IO Val
example = execJet exampleJet (fromList [VNat 3, VNat 4])


-- Ackermann -------------------------------------------------------------------

{-
    ..  $
    |=  x
    |=  y
    ?:  (iszero x)
      (inc y)
    ?:  (iszero y)
      ($ (dec x) 1)
    ($ (dec x) ($ x (dec y)))
-}
ackerJet ∷ Jet
ackerJet = Jet
    { jArgs = 2
    , jName = VUni
    , jBody = error "HACK"
    , jFast = exp
    , jRegs = 0
    }
  where
    x = REF 0
    y = REF 1
    exp =
        (IFF (ZER x)
          (INC y)
        (IFF (ZER y)
          (REC2 (FEC x) (VAL $ VNat 1))
        (REC2 (FEC x)
         (REC2 x (FEC y)))))

acker ∷ Nat → Nat → IO Val
acker x y = execJet2 ackerJet (VNat x) (VNat y)
