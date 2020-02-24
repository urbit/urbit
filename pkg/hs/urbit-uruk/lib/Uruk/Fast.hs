{-# OPTIONS_GHC -funbox-strict-fields #-}

{-
    TODO Fill out callNodeFull.
    TODO K should not evaluate tail.
    TODO Implement pattern-match registers.
-}

{-
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

import ClassyPrelude        hiding (evaluate, fromList, try)
import Data.Primitive.Array
import System.IO.Unsafe

import Control.Arrow    ((>>>))
import Data.Bits        (shiftL, (.|.))
import Data.Function    ((&))
import Numeric.Natural  (Natural)
import Numeric.Positive (Positive)
import Prelude          ((!!))
import Uruk.JetDemo     (Ur, UrPoly(Fast))
import Data.Flat

import qualified Urbit.Atom   as Atom
import qualified Uruk.JetComp as Comp
import qualified Uruk.JetDemo as Ur

import qualified GHC.Exts

--------------------------------------------------------------------------------

type Nat = Natural
type Bol = Bool
type Pos = Positive


-- Raw Uruk --------------------------------------------------------------------

data Pri = J | K | S | D
  deriving stock    (Eq, Ord, Show, Generic)
  deriving anyclass (Flat, NFData)

data Raw = Raw !Pri ![Raw]
  deriving stock    (Eq, Ord, Show, Generic)
  deriving anyclass (Flat, NFData)

jamRaw :: Raw -> Val
jamRaw = VNat . Atom.bytesAtom . flat

toRaw :: Val -> Raw
toRaw = valFun >>> \case
  Fun _ f xs -> app (nodeRaw f) (GHC.Exts.toList $ toRaw <$> xs)
 where
  --  Doesn't need to be simplified because input is already fully
  --  evaluated.
  app :: Raw -> [Raw] -> Raw
  app (Raw f xs) mor = Raw f (xs <> mor)

nodeRaw :: Node -> Raw
nodeRaw = \case
  Jay 1 -> Raw J []
  Kay   -> Raw K []
  Ess   -> Raw S []
  Dee   -> Raw D []
  _     -> error "TODO"

priFun :: Pri -> (Int, Node)
priFun = \case
  S -> (3, Seq)
  K -> (2, Kay)
  J -> (1, Jay 1)
  D -> (1, Dee)

rawVal :: Raw -> Val
{-# INLINE rawVal #-}
rawVal (Raw p xs) = VFun $ Fun (args - sizeofArray vals) node vals
 where
  (args, node) = priFun p
  vals         = rawVal <$> fromList xs

jam :: Val -> Val
{-# INLINE jam #-}
jam = jamRaw . toRaw


--------------------------------------------------------------------------------

data Jet = Jet
    { jArgs ∷ !Int
    , jName ∷ Val
    , jBody ∷ Val
    , jFast ∷ !Exp
    , jRegs ∷ !Int -- Number of registers needed.
    }
  deriving (Eq, Ord, Show)

data Node
    = Jay Pos
    | Kay
    | Ess
    | Dee
    | Jut Jet
    | Eye
    | Bee
    | Sea
    | Sen Pos
    | Ben Pos
    | Cen Pos
    | Seq
    | Yet Nat
    | Fix
    | Nat Nat
    | Bol Bool
    | Iff
    | Pak
    | Zer
    | Eql
    | Add
    | Inc
    | Dec
    | Fec
    | Mul
    | Sub
    | Ded
    | Uni
    | Lef
    | Rit
    | Cas
    | Con
    | Car
    | Cdr
  deriving stock (Eq, Ord, Show)

data Fun = Fun
    { fNeed ∷ !Int
    , fHead ∷ !Node
    , fArgs ∷ Array Val -- Lazy on purpose.
    }
  deriving stock (Eq, Ord, Show)

data Val
    = VUni
    | VCon !Val !Val
    | VLef !Val
    | VRit !Val
    | VNat !Nat
    | VBol !Bool
    | VFun !Fun
  deriving (Eq, Ord, Show)

data Exp
    = VAL !Val                 --  Constant Value
    | REF !Int                 --  Stack Reference
    | REC1 !Exp                --  Recursive Call (one args)
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

    | CON !Exp !Exp            --  Cons
    | CAR !Exp                 --  Head
    | CDR !Exp                 --  Tail
    | LEF !Exp                 --  Left Constructor
    | RIT !Exp                 --  Right Constructor

    | JETN !Jet !(Array Exp)   --  Fully saturated call
    | JET2 !Jet !Exp !Exp      --  Fully saturated call
    | CLON !Fun !(Array Exp)   --  Undersaturated call
    | CALN !Exp !(Array Exp)   --  Call of unknown saturation
  deriving (Eq, Ord, Show)


--------------------------------------------------------------------------------

data TypeError = TypeError Text
  deriving (Eq, Ord, Show, Exception)

data Crash = Crash Val
  deriving (Eq, Ord, Show, Exception)

data BadRef = BadRef Jet Int
  deriving (Eq, Ord, Show, Exception)

type IOArray = MutableArray RealWorld

emptyRegisterSet :: IOArray Val
emptyRegisterSet = unsafePerformIO (newArray 0 VUni)


--------------------------------------------------------------------------------

{- |
    If a function is oversaturated, call it (with too many arguments,
    it's fine), and then call the result with the remaining arguments.
-}
execFun :: Fun -> IO Val
{-# INLINE execFun #-}
execFun f@Fun {..} = compare fNeed 0 & \case
  GT -> pure (VFun f)
  EQ -> callNodeFull fHead fArgs
  LT -> do
    f         <- callNodeFull fHead fArgs
    extraArgs <- arrayDrop (sizeofArray fArgs) (negate fNeed) fArgs
    callVal f extraArgs

arrayDrop :: Int -> Int -> Array Val -> IO (Array Val)
{-# INLINE arrayDrop #-}
arrayDrop i l xs = thawArray xs i l >>= unsafeFreezeArray

callNodeFull :: Node -> Array Val -> IO Val
{-# INLINE callNodeFull #-}
callNodeFull !no !xs = no & \case
  Kay   -> pure x
  Dee   -> pure $ jam x
  Ess   -> join (c2 x z <$> c1 y z)
  Jay _ -> error "TODO"
  _     -> error "TODO"
  where
   x = v 0
   y = v 1
   z = v 2
   v = indexArray xs

c1 = callVal1
c2 = callVal2

callVal1 f x   = callVal f (fromList [x])
callVal2 f x y = callVal f (fromList [x,y])

callFunFull :: Fun -> Array Val -> IO Val
{-# INLINE callFunFull #-}
callFunFull Fun {..} xs = callNodeFull fHead (fArgs <> xs)

valFun :: Val -> Fun
{-# INLINE valFun #-}
valFun = \case
  VUni     -> Fun 1 Uni mempty
  VCon h t -> Fun 1 Con (fromList [h, t])
  VLef l   -> Fun 2 Lef (fromList [l])
  VRit r   -> Fun 2 Lef (fromList [r])
  VNat n   -> Fun 2 (Nat n) mempty
  VBol b   -> Fun 2 (Bol b) mempty
  VFun f   -> f

execJet :: Jet -> Array Val -> IO Val
{-# INLINE execJet #-}
execJet !j !xs = do
  regs <- case jRegs j of
    0 -> pure emptyRegisterSet
    n -> newArray n VUni

  handle (\(TypeError x) -> runSlow x) $ execJetBody j xs regs

 where
  runSlow why = do
    putStrLn ("FALLBACK: " <> why)
    callFunFull (valFun $ jBody j) xs

execJet2 :: Jet -> Val -> Val -> IO Val
{-# INLINE execJet2 #-}
execJet2 !j !x !y = do
  handle (\(TypeError x) -> runSlow x) $ execJetBody2 j x y
 where
  runSlow why = do
    putStrLn ("FALLBACK: " <> why)
    callFunFull (valFun $ jBody j) (fromList [x, y])

execJet1 :: Jet -> Val -> IO Val
{-# INLINE execJet1 #-}
execJet1 !j !x = do
  handle (\(TypeError x) -> runSlow x) $ execJetBody1 j x
 where
  runSlow why = do
    putStrLn ("FALLBACK: " <> why)
    callFunFull (valFun $ jBody j) (fromList [x])


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

inc :: (Exp -> IO Val) -> Exp -> IO Val
{-# INLINE inc #-}
inc go x = go x >>= \case
  VNat x -> pure $ VNat (x + 1)
  _      -> throwIO (TypeError "inc-not-nat")

dec :: (Exp -> IO Val) -> Exp -> IO Val
{-# INLINE dec #-}
dec go x = go x >>= \case
  VNat 0 -> pure $ VLef VUni
  VNat n -> pure $ VRit (VNat (n - 1))
  _      -> throwIO (TypeError "dec-not-nat")

fec :: (Exp -> IO Val) -> Exp -> IO Val
{-# INLINE fec #-}
fec go x = go x >>= \case
  VNat 0 -> pure $ VNat 0
  VNat n -> pure $ VNat (n - 1)
  _      -> throwIO (TypeError "fec-not-nat")

--------------------------------------------------------------------------------


execJetBody :: Jet -> Array Val -> IOArray Val -> IO Val
{-# INLINE execJetBody #-}
execJetBody !j !xs !regs = go (jFast j)
 where
  setReg :: Int -> Val -> IO ()
  setReg i x = writeArray regs i x

  go :: Exp -> IO Val
  go = \case
    VAL v       -> pure v
    REF i       -> pure $ indexArray xs i
    REC1 x      -> join (execJet1 j <$> go x)
    REC2 x y    -> join (execJet2 j <$> go x <*> go y)
    RECN xs     -> join (execJet j <$> traverse go xs)
    SLF         -> pure (jetVal j)
    IFF c t e   -> iff go c t e
    CAS i x l r -> cas go setReg i x l r
    SEQ x y     -> go x >> go y
    DED x       -> throwIO . Crash =<< go x
    INC x       -> inc go x
    DEC x       -> dec go x
    FEC x       -> fec go x
    LEF x       -> VLef <$> go x
    RIT x       -> VRit <$> go x
    JET2 j x y  -> jet2 go j x y
    JETN j xs   -> jetN go j xs

    ADD  x y    -> (,) <$> go x <*> go y >>= \case
      (VNat x, VNat y) -> pure (VNat (x + y))
      (_     , _     ) -> throwIO (TypeError "add-not-nat")

    MUL x y -> (,) <$> go x <*> go y >>= \case
      (VNat x, VNat y) -> pure (VNat (x * y))
      (_     , _     ) -> throwIO (TypeError "mul-not-nat")

    SUB x y -> (,) <$> go x <*> go y >>= \case
      (VNat x, VNat y) | y > x -> pure (VLef VUni)
      (VNat x, VNat y)         -> pure (VRit (VNat (x - y)))
      (_     , _     )         -> throwIO (TypeError "sub-not-nat")

    ZER x -> go x >>= \case
      VNat 0 -> pure (VBol True)
      VNat n -> pure (VBol False)
      xv     -> throwIO (TypeError ("zer-not-nat: " <> tshow xv))

    EQL x y -> (,) <$> go x <*> go y >>= \case
      (VNat x, VNat y) -> pure (VBol (x == y))
      (_     , _     ) -> throwIO (TypeError "eql-not-nat")

    CON x y -> VCon <$> go x <*> go y

    CAR x   -> go x >>= \case
      VCon x _ -> pure x
      _        -> throwIO (TypeError "car-not-con")

    CDR x -> go x >>= \case
      VCon _ y -> pure y
      _        -> throwIO (TypeError "cdr-not-con")

    CLON Fun {..} xs -> do
      xs <- traverse go xs
      let rem :: Int = fNeed - sizeofArray xs
      pure $ VFun $ Fun rem fHead (fArgs <> xs)

    CALN f xs -> do
      fv <- go f
      xs <- traverse go xs
      callVal fv xs

callVal :: Val -> Array Val -> IO Val
callVal f xs =
  let Fun {..} = valFun f
  in  execFun (Fun (fNeed - sizeofArray xs) fHead (fArgs <> xs))


--------------------------------------------------------------------------------

jetVal :: Jet -> Val
jetVal j = VFun $ Fun (jArgs j) (Jut j) mempty

execJetBody2 :: Jet -> Val -> Val -> IO Val
{-# INLINE execJetBody2 #-}
execJetBody2 !j !x !y = go (jFast j)
 where
  go :: Exp -> IO Val
  go = \case
    VAL v       -> pure v
    REF 0       -> pure x
    REF 1       -> pure y
    REF n       -> throwIO (BadRef j n)
    REC1 x      -> join (execJet1 j <$> go x)
    REC2 x y    -> join (execJet2 j <$> go x <*> go y)
    RECN xs     -> join (execJet j <$> traverse go xs)
    SLF         -> pure (jetVal j)
    IFF c t e   -> iff go c t e
    CAS i x l r -> cas go (error "TODO") i x l r
    SEQ x y     -> go x >> go y
    DED x       -> throwIO . Crash =<< go x
    INC x       -> inc go x
    DEC x       -> dec go x
    FEC x       -> fec go x
    JETN j xs   -> jetN go j xs
    JET2 j x y  -> jet2 go j x y

    ADD x y     -> (,) <$> go x <*> go y >>= \case
      (VNat x, VNat y) -> pure (VNat (x + y))
      (_     , _     ) -> throwIO (TypeError "add-not-nat")

    MUL x y -> (,) <$> go x <*> go y >>= \case
      (VNat x, VNat y) -> pure (VNat (x * y))
      (_     , _     ) -> throwIO (TypeError "mul-not-nat")

    SUB x y -> (,) <$> go x <*> go y >>= \case
      (VNat x, VNat y) | y > x -> pure (VLef VUni)
      (VNat x, VNat y)         -> pure (VRit (VNat (x - y)))
      (_     , _     )         -> throwIO (TypeError "sub-not-nat")

    ZER x -> go x >>= \case
      VNat 0 -> pure (VBol True)
      VNat n -> pure (VBol False)
      xv     -> throwIO (TypeError ("zer-not-nat: " <> tshow xv))

    EQL x y -> (,) <$> go x <*> go y >>= \case
      (VNat x, VNat y) -> pure (VBol (x == y))
      (_     , _     ) -> throwIO (TypeError "eql-not-nat")


    CON x y -> VCon <$> go x <*> go y

    CAR x   -> go x >>= \case
      VCon x _ -> pure x
      _        -> throwIO (TypeError "car-not-con")

    CDR x -> go x >>= \case
      VCon _ y -> pure y
      _        -> throwIO (TypeError "cdr-not-con")

    LEF x            -> VLef <$> go x
    RIT x            -> VRit <$> go x

    CLON Fun {..} xs -> do
      xs <- traverse go xs
      pure $ VFun $ Fun (fNeed - sizeofArray xs) fHead (fArgs <> xs)

    CALN f xs -> do
      fv <- go f
      xs <- traverse go xs
      callVal fv xs


execJetBody1 :: Jet -> Val -> IO Val
{-# INLINE execJetBody1 #-}
execJetBody1 !j !x = go (jFast j)
 where
  go :: Exp -> IO Val
  go = \case
    VAL v       -> pure v
    REF 0       -> pure x
    REF n       -> throwIO (BadRef j n)
    REC1 x      -> join (execJet1 j <$> go x)
    REC2 x y    -> join (execJet2 j <$> go x <*> go y)
    RECN xs     -> join (execJet j <$> traverse go xs)
    SLF         -> pure (jetVal j)
    IFF c t e   -> iff go c t e
    CAS i x l r -> cas go (error "TODO") i x l r
    SEQ x y     -> go x >> go y
    DED x       -> throwIO . Crash =<< go x
    INC x       -> inc go x
    DEC x       -> dec go x
    FEC x       -> fec go x
    JETN j xs   -> jetN go j xs
    JET2 j x y  -> jet2 go j x y

    ADD x y     -> (,) <$> go x <*> go y >>= \case
      (VNat x, VNat y) -> pure (VNat (x + y))
      (_     , _     ) -> throwIO (TypeError "add-not-nat")

    MUL x y -> (,) <$> go x <*> go y >>= \case
      (VNat x, VNat y) -> pure (VNat (x * y))
      (_     , _     ) -> throwIO (TypeError "mul-not-nat")

    SUB x y -> (,) <$> go x <*> go y >>= \case
      (VNat x, VNat y) | y > x -> pure (VLef VUni)
      (VNat x, VNat y)         -> pure (VRit (VNat (x - y)))
      (_     , _     )         -> throwIO (TypeError "sub-not-nat")

    ZER x -> go x >>= \case
      VNat 0 -> pure (VBol True)
      VNat n -> pure (VBol False)
      xv     -> throwIO (TypeError ("zer-not-nat: " <> tshow xv))

    EQL x y -> (,) <$> go x <*> go y >>= \case
      (VNat x, VNat y) -> pure (VBol (x == y))
      (_     , _     ) -> throwIO (TypeError "eql-not-nat")


    CON x y -> VCon <$> go x <*> go y

    CAR x   -> go x >>= \case
      VCon x _ -> pure x
      _        -> throwIO (TypeError "car-not-con")

    CDR x -> go x >>= \case
      VCon _ y -> pure y
      _        -> throwIO (TypeError "cdr-not-con")

    LEF x            -> VLef <$> go x
    RIT x            -> VRit <$> go x

    CLON Fun {..} xs -> do
      xs <- traverse go xs
      pure $ VFun $ Fun (fNeed - sizeofArray xs) fHead (fArgs <> xs)

    CALN f xs -> do
      fv <- go f
      xs <- traverse go xs
      callVal fv xs


-- Trivial Example -------------------------------------------------------------

exampleJet :: Jet
exampleJet = Jet { jArgs = 2
                 , jName = error "HACK"
                 , jBody = error "HACK"
                 , jFast = exp
                 , jRegs = 0
                 }
  where exp = ADD (REF 0) (ADD (REF 1) (REF 1))

example :: IO Val
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
ackerJet :: Jet
ackerJet = Jet { jArgs = 2
               , jName = VUni
               , jBody = error "HACK"
               , jFast = exp
               , jRegs = 0
               }
 where
  x = REF 0
  y = REF 1
  exp =
    (IFF
      (ZER x)
      (INC y)
      (IFF (ZER y) (REC2 (FEC x) (VAL $ VNat 1)) (REC2 (FEC x) (REC2 x (FEC y)))
      )
    )

acker :: Nat -> Nat -> IO Val
acker x y = execJet2 ackerJet (VNat x) (VNat y)
