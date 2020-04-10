{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -funbox-strict-fields -Werror #-}

{-
    Note that On 64 bit machines, GHC will always use pointer tagging
    as long as there are less than 8 constructors. So, anything that is
    frequently pattern matched on should have at most 7 branches.

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

module Urbit.UrukRTS where

import ClassyPrelude             hiding (evaluate, fromList, seq, toList, try)
import Control.Monad.Primitive
#if !defined(__GHCJS__)
import Data.Flat
#endif
import Data.Primitive.Array
import Data.Primitive.SmallArray
import GHC.Prim                  hiding (seq)
import System.IO.Unsafe
import Urbit.Moon.Arity
import Urbit.Uruk.Class
import Urbit.UrukRTS.Types

import Control.Arrow     ((>>>))
import Control.Exception (throw, try)
import Data.Bits         (shiftL, (.|.))
import Data.Function     ((&))
import GHC.Exts          (fromList, toList)
import Numeric.Natural   (Natural)
import Prelude           ((!!))
import Text.Show.Pretty  (pPrint, ppShow)

import qualified Urbit.Atom                as Atom
import qualified Urbit.UrukRTS.JetOptimize as Opt
import qualified Urbit.UrukRTS.OptToFast   as Opt


--------------------------------------------------------------------------------

mkNode :: Int -> Node -> Val
mkNode n c = VFun (Fun n c mempty)

instance Uruk Val where
  uApp x y = kVV x y

  uEss = mkNode 3 Ess
  uKay = mkNode 2 Kay
  uJay = \n -> mkNode 2 $ Jay $ fromIntegral n
  uDee = mkNode 1 Dee

  uEye n = mkNode (fromIntegral $ n) (Eye $ fromIntegral n)
  uBee n = mkNode (fromIntegral $ 2 + n) (Bee $ fromIntegral n)
  uSea n = mkNode (fromIntegral $ 2 + n) (Sea $ fromIntegral n)
  uSen n = mkNode (fromIntegral $ 2 + n) (Sen $ fromIntegral n)

  uNat = \n -> VNat n
  uBol = \b -> VBol b

  uUni = mkNode 1 Uni
  uCon = mkNode 2 Con -- hack, actually 3
  uSeq = mkNode 2 Seq
  uCas = mkNode 3 Cas
  uFix = mkNode 2 Fix
  uIff = mkNode 3 Iff

  -- TODO XX HACK (Need to classify nodes)
  -- TODO XX HACK Need to fix arities for value constructors (con/lef/rit)
  uArity = Just . AriOth . fromIntegral . fNeed . valFun

  uGlobal "add" = Just $ mkNode 2 Add
  uGlobal "lef" = Just $ mkNode 1 Lef -- hack, actually 3
  uGlobal "rit" = Just $ mkNode 1 Rit -- hack, actually 3
  uGlobal "pak" = Just $ mkNode 1 Pak
  uGlobal "zer" = Just $ mkNode 1 Zer
  uGlobal "eql" = Just $ mkNode 2 Eql
  uGlobal "inc" = Just $ mkNode 1 Inc
  uGlobal "dec" = Just $ mkNode 1 Dec
  uGlobal "fec" = Just $ mkNode 1 Fec
  uGlobal "ded" = Just $ mkNode 1 Ded
  uGlobal "car" = Just $ mkNode 1 Car
  uGlobal "cdr" = Just $ mkNode 1 Cdr
  uGlobal "sub" = Just $ mkNode 2 Sub
  uGlobal "mul" = Just $ mkNode 2 Mul

  uGlobal "lsh"   = Just $ mkNode 2 Lsh
  uGlobal "lth"   = Just $ mkNode 2 Lth
  uGlobal "fub"   = Just $ mkNode 2 Fub
  uGlobal "not"   = Just $ mkNode 1 Not
  uGlobal "xor"   = Just $ mkNode 2 Xor
  uGlobal "div"   = Just $ mkNode 2 Div
  uGlobal "trace" = Just $ mkNode 2 Tra
  uGlobal "mod"   = Just $ mkNode 2 Mod

  uGlobal _     = Nothing


-- Useful Types ----------------------------------------------------------------

type Nat = Natural
type Bol = Bool


-- Raw Uruk (Basically just used for D (jam)) ----------------------------------

data Pri = J | K | S | D
  deriving stock    (Eq, Ord, Show, Generic)
#if defined(__GHCJS__)
  deriving anyclass (NFData)
#else
  deriving anyclass (Flat, NFData)
#endif

data Raw = Raw !Pri ![Raw]
  deriving stock    (Eq, Ord, Show, Generic)
#if defined(__GHCJS__)
  deriving anyclass (NFData)
#else
  deriving anyclass (Flat, NFData)
#endif

jamRaw :: Raw -> Val
jamRaw =
#if defined(__GHCJS__)
  error "jamRaw depends on `flat`. Get it working in GHCJS."
#else
  VNat . Atom.bytesAtom . flat
#endif

{-
    Note that it's safe for `app` to simply append arguments without
    simplification because we take a `Val` as an argument, which is
    guaranteed to already be in normal form.
-}
toRaw :: Val -> Raw
toRaw = valFun >>> \case
  Fun _ f xs -> app (nodeRaw f) $ toList $ fmap toRaw xs
 where
  app :: Raw -> [Raw] -> Raw
  app (Raw f xs) mor = Raw f (xs <> mor)

nodeRaw :: Node -> Raw
nodeRaw = \case
  Jay 1 -> Raw J []
  Kay   -> Raw K []
  Ess   -> Raw S []
  Dee   -> Raw D []
  n     -> error ("TODO: nodeRaw." <> show n)

priFun :: Pri -> (Int, Node)
priFun = \case
  S -> (3, Ess)
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


--------------------------------------------------------------------------------

arrayDrop :: Int -> Int -> CloN -> IO CloN
{-# INLINE arrayDrop #-}
arrayDrop i l xs = thawSmallArray xs i l >>= unsafeFreezeSmallArray

fixClo :: Val -> Val
fixClo x = VFun (Fun 1 Fix (fromList [x])) -- TODO Slow

indent = unlines . fmap ("    | " <>) . lines

jetRegister :: Int -> Val -> Val -> IO Val
jetRegister args name body = do
  putStrLn "JET REGISTRATION"

  cod <- Opt.compile args name body
  let jet = Opt.optToFast cod

  putStrLn ("  args: " <> tshow args)
  putStrLn ("  name: " <> tshow jet)

  putStrLn ("  body:")
  putStrLn (indent $ pack $ ppShow body)

  putStrLn "  code:"
  putStrLn (indent (pack $ ppShow cod))

  putStrLn "  fast:"
  putStrLn (indent $ pack $ ppShow $ jFast jet)

  pure (VFun (Fun args (Jut jet) mempty))


{-
  TODO Need to handle TypeError exceptions here as well.
-}
reduce :: Node -> CloN -> IO Val
{-# INLINE reduce #-}
reduce !no !xs = do
  let fun = Fun 0 no xs

  -- print no

  res <- no & \case
    Ess   -> kVVA x z (kVV y z)
    Kay   -> pure x
    Jay n -> case x of
      VFun (Fun 2 (Jay 1) _) -> pure (VFun (Fun 1 (Jay (n + 1)) (clo1 y)))
      _                      -> jetRegister n x y

    Dee       -> pure $ jam x

    Add       -> add x y
    Mul       -> mul x y
    Lsh       -> dLsh x y

    Lth       -> dLth x y
    Sub       -> sub x y
    Fub       -> dFub x y
    Not       -> dNot x
    Xor       -> dXor x y
    Div       -> dDiv x y
    Tra       -> dTra x y
    Mod       -> dMod x y


    Inc       -> inc x
    Dec       -> dec x
    Fec       -> fec x
    Seq       -> seq x y
    Bol True  -> pure x
    Bol False -> pure y
    Eql       -> eql x y
    Lef       -> pure (VLef x)
    Rit       -> pure (VRit x)
    Con       -> pure (VCon x y)
    Car       -> car x
    Cdr       -> cdr x
    Cas       -> cas x y z
    Nat n     -> nat n x y

    --  S₁fgx   = (fx)(gx)
    --  S₂fgxy  = (fxy)(gxy)
    --  S₃fgxyz = (fxyz)(gxyz)
    Sen n     -> join (kVA <$> kVVn x args <*> pure (kVVn y args))
     where args = drop 2 $ toList xs

    --  B₁fgx   = f(gx)
    --  B₂fgxy  = f(gxy)
    --  B₃fgxyz = f(gxyz)
    Bee n -> kVA x (kVVn y (drop 2 $ toList xs))

    --  C₁fgx   = (fx)g
    --  C₂fgxy  = (fxy)g
    --  C₃fgxyz = (fxyz)g
    Sea n -> join (kVV <$> (kVVn x (drop 2 $ toList xs)) <*> pure y)

    Eye _ -> toList xs & \case
      []     -> error "impossible"
      [ v ]  -> pure v
      v : vs -> kVVn v vs

    Pak   -> pak x
    Uni   -> pure x

    Ded   -> throwIO (Crash x)
    Fix   -> fix x y

    Zer   -> zer x

    Iff   -> dIff x y z

    Jut j -> execJetN j xs

  -- putStrLn ("  in: ")
  -- putStrLn (indent (pack (ppShow fun)))
  -- putStrLn ("  out:")
  -- putStrLn (indent (pack (ppShow res)))

  pure res
 where
  v         = indexSmallArray xs
  (x, y, z) = (v 0, v 1, v 2)

kFV :: Fun -> Val -> IO Val
{-# INLINE kFV #-}
kFV f x = f & \case
  Fun 1    node args -> reduce node (addCloN args x)
  Fun need head args -> pure $ VFun $ Fun (need-1) head (addCloN args x)

kVAA :: Val -> IO Val -> IO Val -> IO Val
{-# INLINE kVAA #-}
kVAA f x y = kVAn f (fromList [x, y])

kVA :: Val -> IO Val -> IO Val
{-# INLINE kVA #-}
kVA f x = f & \case
  VFun (Fun 1 Kay xs) -> kVV f VUni  --  second arg always ignored.
  other               -> kVV f =<< x

kVVn :: Val -> [Val] -> IO Val
{-# INLINE kVVn #-}
kVVn f xs = foldM kVV f xs

kVAn :: Val -> ArgN -> IO Val
{-# INLINE kVAn #-}
kVAn f xs = foldM kVA f (toList xs)

kVV :: Val -> Val -> IO Val
{-# INLINE kVV #-}
kVV = kFV . valFun

kVVV :: Val -> Val -> Val -> IO Val
{-# INLINE kVVV #-}
kVVV x y z = do
  xy <- kVV x y
  kVV xy z

kVVA :: Val -> Val -> IO Val -> IO Val
{-# INLINE kVVA #-}
kVVA x y z = do
  xy <- kVV x y
  kVA xy z

callFunFull :: Fun -> CloN -> IO Val
{-# INLINE callFunFull #-}
callFunFull Fun {..} xs = reduce fHead (fArgs <> xs)


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
withFallback j args act = do
  res <- act
  -- traceResu j (toList args) res
  pure res


{- catch act $ \(TypeError why) -> do
  putStrLn ("FALLBACK: " <> why)
  callFunFull (valFun $ jBody j) args
-}

traceCall :: Jet -> [Val] -> Bool -> IO ()
traceCall j xs reg = putStrLn ("CALL (" <> body <> ")")
 where
  body = intercalate " " (tshow j <> note : fmap tshow xs)
  note = if reg && jRegs j /= 0
         then "{" <> tshow (jRegs j) <> "}"
         else ""

traceResu :: Jet -> [Val] -> Val -> IO ()
traceResu j xs val = putStrLn ("RETR (" <> body <> ")\n  ==>  " <> tshow val)
 where
  body = intercalate " " (tshow j : fmap tshow xs)

execJet1 :: Jet -> Val -> IO Val
execJet1 !j !x = do
  -- traceCall j [x] False
  (reg, setReg) <- mkRegs (jRegs j)
  let args = fromList [x]
  let refr = \case
        0 -> pure x
        n -> throwIO (BadRef j n)
  withFallback j args (execJetBody j refr reg setReg)

execJet1R :: Jet -> Val -> IO Val
execJet1R !j !x = do
  -- traceCall j [x] True
  let args = fromList [x]
  let refr = \case
        0 -> pure x
        n -> throwIO (BadRef j n)
  withFallback j args (execJetBodyR j refr)

execJet2 :: Jet -> Val -> Val -> IO Val
execJet2 !j !x !y = do
  -- traceCall j [x,y] False
  (reg, setReg) <- mkRegs (jRegs j)
  let args = fromList [x, y]
  let refr = \case
        0 -> pure x
        1 -> pure y
        n -> throwIO (BadRef j n)
  withFallback j args (execJetBody j refr reg setReg)

execJet2R :: Jet -> Val -> Val -> IO Val
execJet2R !j !x !y = do
  -- traceCall j [x,y] True
  let args = fromList [x, y]
  let refr = \case
        0 -> pure x
        1 -> pure y
        n -> throwIO (BadRef j n)
  withFallback j args (execJetBodyR j refr)

execJet3 :: Jet -> Val -> Val -> Val -> IO Val
execJet3 !j !x !y !z = do
  -- traceCall j [x,y,z] False
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
  -- traceCall j [x,y,z] True
  let args = fromList [x, y, z]
  let refr = \case
        0 -> pure x
        1 -> pure y
        2 -> pure z
        n -> throwIO (BadRef j n)
  withFallback j args (execJetBodyR j refr)

execJet4 :: Jet -> Val -> Val -> Val -> Val -> IO Val
execJet4 !j !x !y !z !p = do
  -- traceCall j [x,y,z,p] False
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
  -- traceCall j [x,y,z,p] True
  let args = fromList [x, y, z, p]
  let refr = \case
        0 -> pure x
        1 -> pure y
        2 -> pure z
        3 -> pure p
        n -> throwIO (BadRef j n)
  withFallback j args (execJetBodyR j refr)

execJet5 :: Jet -> Val -> Val -> Val -> Val -> Val -> IO Val
execJet5 !j !x !y !z !p !q = do
  -- traceCall j [x,y,z,p] False
  (reg, setReg) <- mkRegs (jRegs j)
  let args = fromList [x, y, z, p, q]
  let refr = \case
        0 -> pure x
        1 -> pure y
        2 -> pure z
        3 -> pure p
        4 -> pure q
        n -> throwIO (BadRef j n)
  withFallback j args (execJetBody j refr reg setReg)

execJet5R :: Jet -> Val -> Val -> Val -> Val -> Val -> IO Val
execJet5R !j !x !y !z !p !q = do
  -- traceCall j [x,y,z,p] True
  let args = fromList [x, y, z, p, q]
  let refr = \case
        0 -> pure x
        1 -> pure y
        2 -> pure z
        3 -> pure p
        4 -> pure q
        n -> throwIO (BadRef j n)
  withFallback j args (execJetBodyR j refr)


execJetN :: Jet -> CloN -> IO Val
execJetN !j !xs = do
  -- traceCall j (toList xs) (jRegs j /= 0)
  (reg, setReg) <- mkRegs (jRegs j)
  let refr = pure . indexSmallArray xs
  withFallback j xs (execJetBody j refr reg setReg)

execJetNR :: Jet -> CloN -> IO Val
execJetNR !j !xs = do
  -- traceCall j (toList xs) (jRegs j /= 0)
  let refr = pure . indexSmallArray xs
  withFallback j xs (execJetBodyR j refr)


-- Primitive Implementation ----------------------------------------------------

fix :: Val -> Val -> IO Val
{-# INLINE fix #-}
fix x y = kVVV x (fixClo x) y

dIff :: Val -> Val -> Val -> IO Val
{-# INLINE dIff #-}
dIff (VBol True)  t e = kVV t VUni
dIff (VBol False) t e = kVV e VUni
dIff c            t e = do
  print ("dIff", c, t, e)
  throwIO (TypeError "iff-not-bol")

cas :: Val -> Val -> Val -> IO Val
{-# INLINE cas #-}
cas (VLef x) l r = kVV l x
cas (VRit x) l r = kVV r x
cas c        _ _ = do
  pPrint c
  throwIO (TypeError "cas-not-sum")

seq :: Val -> Val -> IO Val
{-# INLINE seq #-}
seq x y = pure y

nat :: Natural -> Val -> Val -> IO Val
{-# INLINE nat #-}
nat n inc zer = go n
 where
  go = \case
    0 -> pure zer
    n -> kVA inc (go (n-1))

pak :: Val -> IO Val
{-# INLINE pak #-}
pak (VNat n) = pure (VNat n)
pak _        = throwIO (TypeError "pak-not-nat") -- TODO Probably actually handle this.

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
fec n        = throwIO (TypeError ("fec-not-nat: " <> tshow n))

add :: Val -> Val -> IO Val
{-# INLINE add #-}
add (VNat x) (VNat y) = pure (VNat (x + y))
add _        _        = throwIO (TypeError "add-not-nat")

mul :: Val -> Val -> IO Val
{-# INLINE mul #-}
mul (VNat x) (VNat y) = pure (VNat (x * y))
mul _        _        = throwIO (TypeError "mul-not-nat")

dLsh :: Val -> Val -> IO Val
{-# INLINE dLsh #-}
dLsh (VNat x) (VNat n) = pure (VNat $ shiftL n $ fromIntegral x)
dLsh _        _        = throwIO (TypeError "lsh-not-nat")

dLth :: Val -> Val -> IO Val
{-# INLINE dLth #-}
dLth (VNat x) (VNat y) = pure (VBol (x < y))
dLth _        _        = throwIO (TypeError "lth-not-nat")

dNot :: Val -> IO Val
{-# INLINE dNot #-}
dNot (VBol b) = pure (VBol $ not b)
dNot _        = throwIO (TypeError "not-not-bol")

dMod :: Val -> Val -> IO Val
{-# INLINE dMod #-}
dMod (VNat x) (VNat y) = pure (VNat (x `mod` y))
dMod _        _        = throwIO (TypeError "mod-not-nat")

dXor :: Val -> Val -> IO Val
{-# INLINE dXor #-}
dXor (VBol True ) (VBol True ) = pure (VBol False)
dXor (VBol False) (VBol True ) = pure (VBol True)
dXor (VBol True ) (VBol False) = pure (VBol True)
dXor (VBol False) (VBol False) = pure (VBol False)
dXor _            _            = throwIO (TypeError "xor-not-bol")

dDiv :: Val -> Val -> IO Val
{-# INLINE dDiv #-}
dDiv (VNat x) (VNat y) = pure (VNat (x `div` y))
dDiv x        y        = do
  print ("div",x,y)
  throwIO (TypeError "div-not-nat")

dTra :: Val -> Val -> IO Val
{-# INLINE dTra #-}
dTra x y = do
  putStrLn ("TRACE: " <> tshow x)
  kVV y VUni

sub :: Val -> Val -> IO Val
{-# INLINE sub #-}
sub (VNat x) (VNat y) | y > x = pure (VLef VUni)
sub (VNat x) (VNat y)         = pure (VRit (VNat (x - y)))
sub _        _                = throwIO (TypeError "sub-not-nat")

dFub :: Val -> Val -> IO Val
{-# INLINE dFub #-}
dFub (VNat x) (VNat y) | y > x = pure (VNat 0)
dFub (VNat x) (VNat y)         = pure (VNat (x - y))
dFub _        _                = throwIO (TypeError "fub-not-nat")

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
car v          = do
  print v
  throwIO (TypeError "car-not-con")

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
    VAL   v         -> pure v
    REF   i         -> ref i
    REG   i         -> reg i
    REC1  x         -> join (execJet1 j <$> go x)
    REC1R x         -> join (execJet1R j <$> go x)
    REC2  x y       -> join (execJet2 j <$> go x <*> go y)
    REC2R x y       -> join (execJet2R j <$> go x <*> go y)
    REC3  x y z     -> join (execJet3 j <$> go x <*> go y <*> go z)
    REC3R x y z     -> join (execJet3R j <$> go x <*> go y <*> go z)
    REC4  x y z p   -> join (execJet4 j <$> go x <*> go y <*> go z <*> go p)
    REC4R x y z p   -> join (execJet4R j <$> go x <*> go y <*> go z <*> go p)
    REC5  x y z p q -> join (execJet5 j <$> go x <*> go y <*> go z <*> go p <*> go q)
    REC5R x y z p q -> join (execJet5R j <$> go x <*> go y <*> go z <*> go p <*> go q)
    RECN  xs        -> join (execJetN j <$> traverse go xs)
    RECNR xs        -> join (execJetNR j <$> traverse go xs)
    SLF             -> pure (jetVal j)
    SEQ x y         -> go x >> go y
    DED x           -> throwIO . Crash =<< go x
    INC x           -> join (inc <$> go x)
    DEC x           -> join (dec <$> go x)
    FEC x           -> join (fec <$> go x)

    JET1 j x        -> join (execJet1 j <$> go x)
    JET2 j x y      -> join (execJet2 j <$> go x <*> go y)
    JET3 j x y z    -> join (execJet3 j <$> go x <*> go y <*> go z)
    JET4 j x y z p  -> join (execJet4 j <$> go x <*> go y <*> go z <*> go p)
    JET5 j x y z p q -> join (execJet5 j <$> go x <*> go y <*> go z <*> go p <*> go q)
    JETN j xs       -> join (execJetN j <$> traverse go xs)

    ADD  x y        -> join (add <$> go x <*> go y)
    MUL  x y        -> join (mul <$> go x <*> go y)

    LTH  x y        -> join (dLth <$> go x <*> go y)
    LSH  x y        -> join (dLsh <$> go x <*> go y)
    FUB  x y        -> join (dFub <$> go x <*> go y)
    NOT  x          -> join (dNot <$> go x)
    XOR  x y        -> join (dXor <$> go x <*> go y)
    DIV  x y        -> join (dDiv <$> go x <*> go y)
    TRA  x y        -> join (dTra <$> go x <*> go y)
    MOD  x y        -> join (dMod <$> go x <*> go y)

    SUB  x y        -> join (sub <$> go x <*> go y)
    ZER x           -> join (zer <$> go x)
    EQL x y         -> join (eql <$> go x <*> go y)
    CAR x           -> join (car <$> go x)
    CDR x           -> join (cdr <$> go x)
    CLON f xs       -> cloN f <$> traverse go xs
    CALN f xs       -> do { f <- go f; kVAn f (go <$> xs) }
    LEF x           -> VLef <$> go x
    RIT x           -> VRit <$> go x
    CON x y         -> VCon <$> go x <*> go y
    IFF c t e       -> go c >>= \case
      VBol True  -> go t
      VBol False -> go e
      cv         -> do
        print ("iff", cv, t, e)
        print ("iff.cond", c)
        throwIO (TypeError "iff-not-bol")
    CAS i x l r -> go x >>= \case
      VLef lv -> setReg i lv >> go l
      VRit rv -> setReg i rv >> go r
      _       -> throwIO (TypeError "cas-not-sum")

execJetBodyR :: Jet -> (Int -> IO Val) -> IO Val
{-# INLINE execJetBodyR #-}
execJetBodyR !j !ref = go (jFast j)
 where
  go :: Exp -> IO Val
  go = \case
    REG i           -> error "execJetBodyR: unexpected register read"
    CAS i x l r     -> error "execJetBodyR: unexpected register write"
    VAL   v         -> pure v
    REF   i         -> ref i
    REC1  x         -> join (execJet1 j <$> go x)
    REC1R x         -> join (execJet1R j <$> go x)
    REC2  x y       -> join (execJet2 j <$> go x <*> go y)
    REC2R x y       -> join (execJet2R j <$> go x <*> go y)
    REC3  x y z     -> join (execJet3 j <$> go x <*> go y <*> go z)
    REC3R x y z     -> join (execJet3R j <$> go x <*> go y <*> go z)
    REC4  x y z p   -> join (execJet4 j <$> go x <*> go y <*> go z <*> go p)
    REC4R x y z p   -> join (execJet4R j <$> go x <*> go y <*> go z <*> go p)
    REC5  x y z p q -> join (execJet5 j <$> go x <*> go y <*> go z <*> go p <*> go q)
    REC5R x y z p q -> join (execJet5R j <$> go x <*> go y <*> go z <*> go p <*> go q)
    RECN  xs        -> join (execJetN j <$> traverse go xs)
    RECNR xs        -> join (execJetNR j <$> traverse go xs)
    SLF             -> pure (jetVal j)
    SEQ x y         -> go x >> go y
    DED x           -> throwIO . Crash =<< go x
    INC x           -> join (inc <$> go x)
    DEC x           -> join (dec <$> go x)
    FEC x           -> join (fec <$> go x)
    JET1 j x        -> join (execJet1 j <$> go x)
    JET2 j x y      -> join (execJet2 j <$> go x <*> go y)
    JET3 j x y z    -> join (execJet3 j <$> go x <*> go y <*> go z)
    JET4 j x y z p  -> join (execJet4 j <$> go x <*> go y <*> go z <*> go p)
    JET5 j x y z p q -> join (execJet5 j <$> go x <*> go y <*> go z <*> go p <*> go q)
    JETN j xs       -> join (execJetN j <$> traverse go xs)
    ADD  x y        -> join (add <$> go x <*> go y)
    MUL  x y        -> join (mul <$> go x <*> go y)

    LTH  x y       -> join (dLth <$> go x <*> go y)
    LSH  x y       -> join (dLsh <$> go x <*> go y)
    FUB  x y       -> join (dFub <$> go x <*> go y)
    NOT  x         -> join (dNot <$> go x)
    XOR  x y       -> join (dXor <$> go x <*> go y)
    DIV  x y       -> join (dDiv <$> go x <*> go y)
    TRA  x y       -> join (dTra <$> go x <*> go y)
    MOD  x y       -> join (dMod <$> go x <*> go y)

    SUB  x y       -> join (sub <$> go x <*> go y)
    ZER x          -> join (zer <$> go x)
    EQL x y        -> join (eql <$> go x <*> go y)
    CAR x          -> join (car <$> go x)
    CDR x          -> join (cdr <$> go x)
    CLON f xs      -> cloN f <$> traverse go xs
    CALN f xs      -> do { f <- go f; kVAn f (go <$> xs) }
    LEF x          -> VLef <$> go x
    RIT x          -> VRit <$> go x
    CON x y        -> VCon <$> go x <*> go y
    IFF c t e      -> go c >>= \case
      VBol True  -> go t
      VBol False -> go e
      cv         -> do
        print ("iff", cv, t, e)
        print ("iff.cond", c)
        throwIO (TypeError "iff-not-bol")
