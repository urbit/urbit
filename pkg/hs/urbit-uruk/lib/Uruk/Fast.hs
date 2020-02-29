{-

    Alright, I need to figure out how to implement short circuiting K.

    If I call a known function (in a jet) nothing changes.

      This will never by K, I think? Otherwise it would have been
      eliminated by the simplifier.

    If I call an *unknown* function, shit gets weird.

      I need to know: "is this the second argument to K?"

      And that's actually a kinda complicated question. What if I am running:

        =/  foo
        ~/  1  foo
        |=  (x y)
        (x y (inc 3) (inc 4))

        (foo car [K K])

      What's the evaluation order here?

      - First, evaluate `x`.

        - It's `car`.

      - Then, evaluate `y`.

        - It's `[K K]`

      - Then, evaluate `(x y)`.

        - This is an application, so we must first know if the head is
          partially-saturated K.

          - It isn't, it's `car`

        - `(car [K K])` evaluated to `K`.

      - Then, evaluate (K (inc 3)).

        - Here, we are applying K, but it isn't saturated, so we procede
          as normal.

        - We get `(K 4)`, which is a special fucking snowflake.

      - Then we evaluate `((K 4) (inc 3))`:

        - Since `(K 4)` is a special fucking snowflake, we must *not
          evaluate* `(inc 3)`. Instead, we should just return `4`.

      A value is still `val = [node (array val)]`.

      - But a call is now:

        `call1 :: Val -> IO Val -> Val`
        `call2 :: Val -> IO Val -> IO Val -> Val`
        `callN :: Val -> Array (IO Val) -> Val`
-}

{-# OPTIONS_GHC -funbox-strict-fields -Werror #-}

{-
    TODO Fill out reduce.
    TODO K should not evaluate tail.
      TODO Or, find a way to make K not need to short-circuit.

    Note that On 64 bit machines, GHC will always use pointer tagging
    as long as there are less than 8 constructors. So, anything that is
    frequently pattern matched on should have at most 7 branches.
-}

module Uruk.Fast where

import ClassyPrelude             hiding (evaluate, fromList, try)
import Control.Monad.Primitive
import Data.Primitive.Array
import Data.Primitive.SmallArray
import GHC.Prim
import System.IO.Unsafe

import Control.Arrow     ((>>>))
import Control.Exception (throw, try)
import Data.Bits         (shiftL, (.|.))
import Data.Flat
import Data.Function     ((&))
import Numeric.Natural   (Natural)
import Numeric.Positive  (Positive)
import Prelude           ((!!))
import Uruk.JetDemo      (Ur, UrPoly(Fast))

import qualified GHC.Exts
import qualified Urbit.Atom   as Atom
import qualified Uruk.JetDemo as Ur


-- Useful Types ----------------------------------------------------------------

type Nat = Natural
type Bol = Bool
type Pos = Positive


-- Raw Uruk (Basically just used for D (jam)) ----------------------------------

data Pri = J | K | S | D
  deriving stock    (Eq, Ord, Show, Generic)
  deriving anyclass (Flat, NFData)

data Raw = Raw !Pri ![Raw]
  deriving stock    (Eq, Ord, Show, Generic)
  deriving anyclass (Flat, NFData)

jamRaw :: Raw -> Val
jamRaw = VNat . Atom.bytesAtom . flat

{-
    Note that it's safe for `app` to simply append arguments without
    simplification because we take a `Val` as an argument, which is
    guaranteed to already be in normal form.
-}
toRaw :: Val -> Raw
toRaw = valFun >>> \case
  Fun _ f xs -> app (nodeRaw f) $ GHC.Exts.toList $ fmap toRaw xs
 where
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
rawVal (Raw p xs) = VFun $ Fun (args - sizeofSmallArray vals) node vals
 where
  (args, node) = priFun p
  vals         = rawVal <$> fromList xs

jam :: Val -> Val
{-# INLINE jam #-}
jam = jamRaw . toRaw


-- Closure ---------------------------------------------------------------------

type CloN = SmallArray Val

getCloN :: CloN -> Int -> Val
{-# INLINE getCloN #-}
getCloN = indexSmallArray

addCloN :: CloN -> Val -> CloN
{-# INLINE addCloN #-}
addCloN xs x = xs <> GHC.Exts.fromList [x] -- TODO Slow


-- Arguments -------------------------------------------------------------------

type ArgN = SmallArray (IO Val)


-- Registers -------------------------------------------------------------------

type RegN = SmallMutableArray RealWorld Val

newRegN :: Int -> IO RegN
{-# INLINE newRegN #-}
newRegN n = newSmallArray n VUni

getRegN :: RegN -> Int -> IO Val
{-# INLINE getRegN #-}
getRegN = readSmallArray

setRegN :: RegN -> Int -> Val -> IO ()
{-# INLINE setRegN #-}
setRegN = writeSmallArray


-- Types -----------------------------------------------------------------------

data Jet = Jet
  { jArgs :: !Int
  , jName :: Val
  , jBody :: Val
  , jFast :: !Exp
  , jRegs :: !Int -- Number of registers needed.
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
 deriving stock (Eq, Ord)

instance Show Node where
  show = \case
    Jay n     -> replicate (fromIntegral n) 'J'
    Kay       -> "K"
    Ess       -> "S"
    Dee       -> "D"
    Jut j     -> show j
    Eye       -> "I"
    Bee       -> "B"
    Sea       -> "C"
    Sen n     -> "S" <> show n
    Ben n     -> "B" <> show n
    Cen n     -> "C" <> show n
    Seq       -> "SEQ"
    Yet n     -> "YET" <> show n
    Fix       -> "FIX"
    Nat n     -> show n
    Bol True  -> "%.y"
    Bol False -> "%.n"
    Iff       -> "IFF"
    Pak       -> "PAK"
    Zer       -> "ZER"
    Eql       -> "EQL"
    Add       -> "ADD"
    Inc       -> "INC"
    Dec       -> "DEC"
    Fec       -> "FEC"
    Mul       -> "MUL"
    Sub       -> "SUB"
    Ded       -> "DED"
    Uni       -> "UNI"
    Lef       -> "LEF"
    Rit       -> "RIT"
    Cas       -> "CAS"
    Con       -> "CON"
    Car       -> "CAR"
    Cdr       -> "CDR"

data Fun = Fun
  { fNeed :: !Int
  , fHead :: !Node
  , fArgs :: CloN -- Lazy on purpose.
  }
 deriving stock (Eq, Ord)

instance Show Fun where
  show (Fun _ h args) = if sizeofSmallArray args == 0
    then show h
    else mconcat
      ["(", show h <> " ", intercalate " " (show <$> GHC.Exts.toList args), ")"]

data Val
  = VUni
  | VCon !Val !Val
  | VLef !Val
  | VRit !Val
  | VNat !Nat
  | VBol !Bool
  | VFun !Fun
 deriving (Eq, Ord)

instance Show Val where
  show = \case
    VUni       -> "~"
    VCon x y   -> "[" <> show x <> " " <> show y <> "]"
    VLef x     -> "L" <> show x
    VRit x     -> "R" <> show x
    VNat n     -> show n
    VBol True  -> "%.y"
    VBol False -> "%.n"
    VFun f     -> show f

data Exp
  = VAL   !Val                    --  Constant Value
  | REF   !Int                    --  Stack Reference
  | REG   !Int                    --  Register Reference
  | SLF                           --  Self Reference

  | IFF   !Exp !Exp !Exp          --  If-Then-Else
  | CAS   !Int !Exp !Exp !Exp     --  Pattern Match
  | REC1  !Exp                    --  Recursive Call
  | REC1R !Exp                    --  Recursive Call (No Registers)
  | REC2  !Exp !Exp               --  Recursive Call
  | REC2R !Exp !Exp               --  Recursive Call (No Registers)
  | REC3  !Exp !Exp !Exp          --  Recursive Call
  | REC3R !Exp !Exp !Exp          --  Recursive Call (No Registers)
  | REC4  !Exp !Exp !Exp !Exp     --  Recursive Call
  | REC4R !Exp !Exp !Exp !Exp     --  Recursive Call (No Registers)
  | RECN  !(SmallArray Exp)       --  Recursive Call
  | RECNR !(SmallArray Exp)       --  Recursive Call (No Registers)

  | SEQ !Exp !Exp                 --  Evaluate head, return tail
  | DED !Exp                      --  Evaluate argument, then crash.

  | INC !Exp                      --  Increment
  | DEC !Exp                      --  Decrement
  | FEC !Exp                      --  Fast decrement
  | ADD !Exp !Exp                 --  Add
  | MUL !Exp !Exp                 --  Multiply
  | SUB !Exp !Exp                 --  Subtract
  | ZER !Exp                      --  Is Zero?
  | EQL !Exp !Exp                 --  Atom equality.

  | CON !Exp !Exp                 --  Cons
  | CAR !Exp                      --  Head
  | CDR !Exp                      --  Tail
  | LEF !Exp                      --  Left Constructor
  | RIT !Exp                      --  Right Constructor

  | JET1 !Jet !Exp                --  Fully saturated jet call.
  | JET2 !Jet !Exp !Exp           --  Fully saturated jet call.
  | JET3 !Jet !Exp !Exp !Exp      --  Fully saturated jet call.
  | JET4 !Jet !Exp !Exp !Exp !Exp --  Fully saturated jet call.
  | JETN !Jet !(SmallArray Exp)   --  Fully saturated jet call.

  | CLON !Fun !(SmallArray Exp)   --  Undersaturated call
  | CALN !Exp !(SmallArray Exp)   --  Call of unknown saturation
 deriving (Eq, Ord, Show)


-- Exceptions ------------------------------------------------------------------

data TypeError = TypeError Text
 deriving (Eq, Ord, Show, Exception)

data Crash = Crash Val
 deriving (Eq, Ord, Show, Exception)

data BadRef = BadRef Jet Int
 deriving (Eq, Ord, Show, Exception)


--------------------------------------------------------------------------------

arrayDrop :: Int -> Int -> CloN -> IO CloN
{-# INLINE arrayDrop #-}
arrayDrop i l xs = thawSmallArray xs i l >>= unsafeFreezeSmallArray

fixClo :: Val -> Val
fixClo x = VFun (Fun 1 Fix (GHC.Exts.fromList [x])) -- TODO Slow

reduce :: Node -> CloN -> IO Val
{-# INLINE reduce #-}
reduce !no !xs = do
  let funStr = tshow (Fun 0 no xs)

  putStrLn funStr

  res <- no & \case
    Kay   -> pure x
    Dee   -> pure $ jam x
    Ess   -> kVVA x z (kVV y z)
    Jay _ -> error "TODO"
    Add   -> add x y
    Fix   -> kVVV x (fixClo x) y
    node  -> error ("TODO: Implement jets in Uruk.Fast: " <> show node)

  putStrLn ("  in: " <> funStr)
  putStrLn ("    out: " <> tshow res)

  pure res
 where
  x = v 0
  y = v 1
  z = v 2
  v = indexSmallArray xs

kFV :: Fun -> Val -> IO Val
kFV f x = f & \case
  Fun 1    node args -> reduce node (addCloN args x)
  Fun need head args -> pure $ VFun $ Fun (need-1) head (addCloN args x)

kVAA :: Val -> IO Val -> IO Val -> IO Val
kVAA = callVal2

kVA :: Val -> IO Val -> IO Val
kVA f x = f & \case
  VFun (Fun 1 Kay xs) -> kVV f VUni  --  second arg always ignored.
  other               -> kVV f =<< x

kAN :: Val -> ArgN -> IO Val
kAN f xs = foldM kVA f (GHC.Exts.toList xs)

kVV :: Val -> Val -> IO Val
kVV = kFV . valFun

kVVV :: Val -> Val -> Val -> IO Val
kVVV x y z = do
  xy <- kVV x y
  kVV xy z

kVVA :: Val -> Val -> IO Val -> IO Val
kVVA x y z = do
  xy <- kVV x y
  kVA xy z

callVal1 f x = kAN f (fromList [x])
callVal2 f x y = kAN f (fromList [x, y])

callFunFull :: Fun -> CloN -> IO Val
{-# INLINE callFunFull #-}
callFunFull Fun {..} xs = reduce fHead (fArgs <> xs)

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


-- Jet Invokation --------------------------------------------------------------

mkRegs :: Int -> IO (Int -> IO Val, Int -> Val -> IO ())
{-# INLINE mkRegs #-}
mkRegs 0 = pure (pure (error "no-registers"), pure (error "no-registers"))
mkRegs 1 = do
  reg <- newIORef VUni
  let read _ = readIORef reg
  let write _ = writeIORef reg
  pure (read, write)
mkRegs n = do
  regs <- newRegN n
  pure (getRegN regs, setRegN regs)

withFallback :: Jet -> CloN -> IO Val -> IO Val
{-# INLINE withFallback #-}
withFallback j args act = catch act $ \(TypeError why) -> do
  putStrLn ("FALLBACK: " <> why)
  callFunFull (valFun $ jBody j) args

execJet1 :: Jet -> Val -> IO Val
execJet1 !j !x = do
  (reg, setReg) <- mkRegs (jRegs j)
  let args = fromList [x]
  let refr = \case
        0 -> pure x
        n -> throwIO (BadRef j n)
  withFallback j args (execJetBody j refr reg setReg)

execJet1R :: Jet -> Val -> IO Val
execJet1R !j !x = do
  let args = fromList [x]
  let refr = \case
        0 -> pure x
        n -> throwIO (BadRef j n)
  withFallback j args (execJetBodyR j refr)

execJet2 :: Jet -> Val -> Val -> IO Val
execJet2 !j !x !y = do
  (reg, setReg) <- mkRegs (jRegs j)
  let args = fromList [x, y]
  let refr = \case
        0 -> pure x
        1 -> pure y
        n -> throwIO (BadRef j n)
  withFallback j args (execJetBody j refr reg setReg)

execJet2R :: Jet -> Val -> Val -> IO Val
execJet2R !j !x !y = do
  let args = fromList [x, y]
  let refr = \case
        0 -> pure x
        1 -> pure y
        n -> throwIO (BadRef j n)
  withFallback j args (execJetBodyR j refr)

execJet3 :: Jet -> Val -> Val -> Val -> IO Val
execJet3 !j !x !y !z = do
  (reg, setReg) <- mkRegs (jRegs j)
  let args = fromList [x, y, z]
      refr = \case
        0 -> pure x
        1 -> pure y
        2 -> pure z
        n -> throwIO (BadRef j n)
  withFallback j args (execJetBody j refr reg setReg)

execJet3R :: Jet -> Val -> Val -> Val -> IO Val
execJet3R !j !x !y !z = do
  let args = fromList [x, y, z]
  let refr = \case
        0 -> pure x
        1 -> pure y
        2 -> pure z
        n -> throwIO (BadRef j n)
  withFallback j args (execJetBodyR j refr)

execJet4 :: Jet -> Val -> Val -> Val -> Val -> IO Val
execJet4 !j !x !y !z !p = do
  (reg, setReg) <- mkRegs (jRegs j)
  let args = fromList [x, y, z, p]
  let refr = \case
        0 -> pure x
        1 -> pure y
        2 -> pure z
        3 -> pure p
        n -> throwIO (BadRef j n)
  withFallback j args (execJetBody j refr reg setReg)

execJet4R :: Jet -> Val -> Val -> Val -> Val -> IO Val
execJet4R !j !x !y !z !p = do
  let args = fromList [x, y, z, p]
  let refr = \case
        0 -> pure x
        1 -> pure y
        2 -> pure z
        3 -> pure p
        n -> throwIO (BadRef j n)
  withFallback j args (execJetBodyR j refr)

execJetN :: Jet -> CloN -> IO Val
execJetN !j !xs = do
  (reg, setReg) <- mkRegs (jRegs j)
  let refr = pure . indexSmallArray xs
  withFallback j xs (execJetBody j refr reg setReg)

execJetNR :: Jet -> CloN -> IO Val
execJetNR !j !xs = do
  let refr = pure . indexSmallArray xs
  withFallback j xs (execJetBodyR j refr)


-- Primitive Implementation ----------------------------------------------------

inc :: Val -> IO Val
{-# INLINE inc #-}
inc (VNat x) = pure $ VNat (x + 1)
inc _        = throwIO (TypeError "inc-not-nat")

dec :: Val -> IO Val
{-# INLINE dec #-}
dec (VNat 0) = pure $ VLef VUni
dec (VNat n) = pure $ VRit (VNat (n - 1))
dec _        = throwIO (TypeError "dec-not-nat")

fec :: Val -> IO Val
{-# INLINE fec #-}
fec (VNat 0) = pure (VNat 0)
fec (VNat n) = pure (VNat (n - 1))
fec _        = throwIO (TypeError "fec-not-nat")

add :: Val -> Val -> IO Val
{-# INLINE add #-}
add (VNat x) (VNat y) = pure (VNat (x + y))
add _        _        = throwIO (TypeError "add-not-nat")

mul :: Val -> Val -> IO Val
{-# INLINE mul #-}
mul (VNat x) (VNat y) = pure (VNat (x * y))
mul _        _        = throwIO (TypeError "mul-not-nat")

sub :: Val -> Val -> IO Val
{-# INLINE sub #-}
sub (VNat x) (VNat y) | y > x = pure (VLef VUni)
sub (VNat x) (VNat y)         = pure (VRit (VNat (x - y)))
sub _        _                = throwIO (TypeError "sub-not-nat")

zer :: Val -> IO Val
{-# INLINE zer #-}
zer (VNat 0) = pure (VBol True)
zer (VNat n) = pure (VBol False)
zer v        = throwIO (TypeError ("zer-not-nat: " <> tshow v))

eql :: Val -> Val -> IO Val
{-# INLINE eql #-}
eql (VNat x) (VNat y) = pure (VBol (x == y))
eql _        _        = throwIO (TypeError "eql-not-nat")

car :: Val -> IO Val
{-# INLINE car #-}
car (VCon x _) = pure x
car _          = throwIO (TypeError "car-not-con")

cdr :: Val -> IO Val
{-# INLINE cdr #-}
cdr (VCon _ y) = pure y
cdr _          = throwIO (TypeError "cdr-not-con")

-- Interpreter -----------------------------------------------------------------

cloN :: Fun -> CloN -> Val
{-# INLINE cloN #-}
cloN (Fun {..}) xs = VFun $ Fun rem fHead $ fArgs <> xs
  where rem = fNeed - sizeofSmallArray xs

jetVal :: Jet -> Val
{-# INLINE jetVal #-}
jetVal j = VFun $ Fun (jArgs j) (Jut j) mempty

execJetBody
  :: Jet
  -> (Int -> IO Val)
  -> (Int -> IO Val)
  -> (Int -> Val -> IO ())
  -> IO Val
{-# INLINE execJetBody #-}
execJetBody !j !ref !reg !setReg = go (jFast j)
 where
  go :: Exp -> IO Val
  go = \case
    VAL   v        -> pure v
    REF   i        -> ref i
    REG   i        -> reg i
    REC1  x        -> join (execJet1 j <$> go x)
    REC1R x        -> join (execJet1R j <$> go x)
    REC2  x y      -> join (execJet2 j <$> go x <*> go y)
    REC2R x y      -> join (execJet2R j <$> go x <*> go y)
    REC3  x y z    -> join (execJet3 j <$> go x <*> go y <*> go z)
    REC3R x y z    -> join (execJet3R j <$> go x <*> go y <*> go z)
    REC4  x y z p  -> join (execJet4 j <$> go x <*> go y <*> go z <*> go p)
    REC4R x y z p  -> join (execJet4R j <$> go x <*> go y <*> go z <*> go p)
    RECN  xs       -> join (execJetN j <$> traverse go xs)
    RECNR xs       -> join (execJetNR j <$> traverse go xs)
    SLF            -> pure (jetVal j)
    SEQ x y        -> go x >> go y
    DED x          -> throwIO . Crash =<< go x
    INC x          -> join (inc <$> go x)
    DEC x          -> join (dec <$> go x)
    FEC x          -> join (fec <$> go x)
    JET1 j x       -> join (execJet1 j <$> go x)
    JET2 j x y     -> join (execJet2 j <$> go x <*> go y)
    JET3 j x y z   -> join (execJet3 j <$> go x <*> go y <*> go z)
    JET4 j x y z p -> join (execJet4 j <$> go x <*> go y <*> go z <*> go p)
    JETN j xs      -> join (execJetN j <$> traverse go xs)
    ADD  x y       -> join (add <$> go x <*> go y)
    MUL  x y       -> join (mul <$> go x <*> go y)
    SUB  x y       -> join (sub <$> go x <*> go y)
    ZER x          -> join (zer <$> go x)
    EQL x y        -> join (eql <$> go x <*> go y)
    CAR x          -> join (car <$> go x)
    CDR x          -> join (cdr <$> go x)
    CLON f xs      -> cloN f <$> traverse go xs
    CALN f xs      -> do { f <- go f; kAN f (go <$> xs) }
    LEF x          -> VLef <$> go x
    RIT x          -> VRit <$> go x
    CON x y        -> VCon <$> go x <*> go y
    IFF c t e      -> go c >>= \case
      VBol True  -> go t
      VBol False -> go e
      _          -> throwIO (TypeError "iff-not-bol")
    CAS i x l r -> go x >>= \case
      VLef lv -> setReg i lv >> go l
      VRit rv -> setReg i rv >> go r
      _       -> throwIO (TypeError "cas-no-sum")

execJetBodyR :: Jet -> (Int -> IO Val) -> IO Val
{-# INLINE execJetBodyR #-}
execJetBodyR !j !ref = go (jFast j)
 where
  go :: Exp -> IO Val
  go = \case
    REG i          -> error "no-reg"
    CAS i x l r    -> error "no-reg"
    VAL   v        -> pure v
    REF   i        -> ref i
    REC1  x        -> join (execJet1 j <$> go x)
    REC1R x        -> join (execJet1R j <$> go x)
    REC2  x y      -> join (execJet2 j <$> go x <*> go y)
    REC2R x y      -> join (execJet2R j <$> go x <*> go y)
    REC3  x y z    -> join (execJet3 j <$> go x <*> go y <*> go z)
    REC3R x y z    -> join (execJet3R j <$> go x <*> go y <*> go z)
    REC4  x y z p  -> join (execJet4 j <$> go x <*> go y <*> go z <*> go p)
    REC4R x y z p  -> join (execJet4R j <$> go x <*> go y <*> go z <*> go p)
    RECN  xs       -> join (execJetN j <$> traverse go xs)
    RECNR xs       -> join (execJetNR j <$> traverse go xs)
    SLF            -> pure (jetVal j)
    SEQ x y        -> go x >> go y
    DED x          -> throwIO . Crash =<< go x
    INC x          -> join (inc <$> go x)
    DEC x          -> join (dec <$> go x)
    FEC x          -> join (fec <$> go x)
    JET1 j x       -> join (execJet1 j <$> go x)
    JET2 j x y     -> join (execJet2 j <$> go x <*> go y)
    JET3 j x y z   -> join (execJet3 j <$> go x <*> go y <*> go z)
    JET4 j x y z p -> join (execJet4 j <$> go x <*> go y <*> go z <*> go p)
    JETN j xs      -> join (execJetN j <$> traverse go xs)
    ADD  x y       -> join (add <$> go x <*> go y)
    MUL  x y       -> join (mul <$> go x <*> go y)
    SUB  x y       -> join (sub <$> go x <*> go y)
    ZER x          -> join (zer <$> go x)
    EQL x y        -> join (eql <$> go x <*> go y)
    CAR x          -> join (car <$> go x)
    CDR x          -> join (cdr <$> go x)
    CLON f xs      -> cloN f <$> traverse go xs
    CALN f xs      -> do { f <- go f; kAN f (go <$> xs) }
    LEF x          -> VLef <$> go x
    RIT x          -> VRit <$> go x
    CON x y        -> VCon <$> go x <*> go y
    IFF c t e      -> go c >>= \case
      VBol True  -> go t
      VBol False -> go e
      _          -> throwIO (TypeError "iff-not-bol")
