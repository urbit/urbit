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

import ClassyPrelude           hiding (evaluate, fromList, seq, toList, try)
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

import Control.Arrow         ((>>>))
import Control.Concurrent    (threadDelay)
import Control.Exception     (evaluate, throw, try)
import Data.Bits             (shiftL, (.|.))
import Data.Function         ((&))
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import GHC.Exts              (fromList, toList)
import Numeric.Natural       (Natural)
import Prelude               ((!!))
import Text.Show.Pretty      (pPrint, ppShow)

import qualified Data.ByteString           as BS
import qualified Data.Char                 as C
import qualified Data.Store                as Store
import qualified Data.Store.TH             as Store
import qualified System.IO                 as Sys
import qualified Urbit.Atom                as Atom
import qualified Urbit.UrukRTS.Inline      as Opt
import qualified Urbit.UrukRTS.JetOptimize as Opt
import qualified Urbit.UrukRTS.OptToFast   as Opt
import qualified Urbit.UrukRTS.RegOpt      as Opt


-- Profiling Events ------------------------------------------------------------

data Event = Event !Bool !POSIXTime !Jet

data EventDisk = EventDisk !Bool !Word64 !ByteString

Store.makeStore ''EventDisk


--------------------------------------------------------------------------------

mkNode :: Int -> Node -> Val
mkNode n c = VFun (Fun n c mempty)

instance Uruk Val where
  uApp x y = kVV x y

  uEss = mkNode 3 Ess
  uKay = mkNode 2 Kay
  uEnh = \n -> mkNode 2 $ Enh $ fromIntegral n
  uDub = mkNode 6 Dub

  uEye n = mkNode (fromIntegral $ n) (Eye $ fromIntegral n)
  uBee n = mkNode (fromIntegral $ 2 + n) (Bee $ fromIntegral n)
  uSea n = mkNode (fromIntegral $ 2 + n) (Sea $ fromIntegral n)
  uSen n = mkNode (fromIntegral $ 2 + n) (Sen $ fromIntegral n)

  uNat   = \n -> VNat n
  uBol   = \b -> VBol b

  uUni   = mkNode 1 Uni
  uCon   = mkNode 2 Con -- hack, actually 3
  uSeq   = mkNode 2 Seq
  uCas   = mkNode 3 Cas
  uLet   = mkNode 2 Let
  uFix   = mkNode 2 Fix
  uIff   = mkNode 3 Iff

  -- TODO XX HACK (Need to classify nodes)
  -- TODO XX HACK Need to fix arities for value constructors (con/lef/rit)
  uArity = Just . AriOth . fromIntegral . fNeed . valFun

  uGlobal "add"          = Just $ mkNode 2 Add
  uGlobal "lef"          = Just $ mkNode 1 Lef -- hack, actually 3
  uGlobal "rit"          = Just $ mkNode 1 Rit -- hack, actually 3
  uGlobal "pak"          = Just $ mkNode 1 Pak
  uGlobal "zer"          = Just $ mkNode 1 Zer
  uGlobal "eql"          = Just $ mkNode 2 Eql
  uGlobal "inc"          = Just $ mkNode 1 Inc
  uGlobal "dec"          = Just $ mkNode 1 Dec
  uGlobal "fec"          = Just $ mkNode 1 Fec
  uGlobal "ded"          = Just $ mkNode 1 Ded
  uGlobal "car"          = Just $ mkNode 1 Car
  uGlobal "cdr"          = Just $ mkNode 1 Cdr
  uGlobal "sub"          = Just $ mkNode 2 Sub
  uGlobal "mul"          = Just $ mkNode 2 Mul

  uGlobal "lsh"          = Just $ mkNode 2 Lsh
  uGlobal "lth"          = Just $ mkNode 2 Lth
  uGlobal "fub"          = Just $ mkNode 2 Fub
  uGlobal "not"          = Just $ mkNode 1 Not
  uGlobal "xor"          = Just $ mkNode 2 Xor
  uGlobal "div"          = Just $ mkNode 2 Div
  uGlobal "trace"        = Just $ mkNode 1 Tra
  uGlobal "mod"          = Just $ mkNode 2 Mod

  uGlobal "let"          = Just $ mkNode 2 Let
  uGlobal "rap"          = Just $ mkNode 2 Rap
  uGlobal "zing"         = Just $ mkNode 1 Zing
  uGlobal "ntot"         = Just $ mkNode 1 Ntot

  uGlobal "int-positive" = Just $ mkNode 1 IntPositive
  uGlobal "int-negative" = Just $ mkNode 1 IntNegative

  uGlobal "int-abs"      = Just $ mkNode 2 IntAbs
  uGlobal "int-add"      = Just $ mkNode 2 IntAdd
  uGlobal "int-div"      = Just $ mkNode 2 IntDiv
  uGlobal "int-is-zer"   = Just $ mkNode 1 IntIsZer
  uGlobal "int-is-neg"   = Just $ mkNode 1 IntIsNeg
  uGlobal "int-is-pos"   = Just $ mkNode 1 IntIsPos
  uGlobal "int-lth"      = Just $ mkNode 2 IntLth
  uGlobal "int-mul"      = Just $ mkNode 2 IntMul
  uGlobal "int-negate"   = Just $ mkNode 1 IntNegate
  uGlobal "int-sub"      = Just $ mkNode 2 IntSub

  uGlobal "box"          = Just $ mkNode 1 MkBox
  uGlobal "unbox"        = Just $ mkNode 2 Unbox

  uGlobal "lcon"         = Just $ mkNode 2 LCon      -- hack, actually 4
  uGlobal "lnil"         = Just $ mkNode 2 (Lis [])
  uGlobal "gulf"         = Just $ mkNode 2 Gulf
  uGlobal "snag"         = Just $ mkNode 2 Snag
  uGlobal "turn"         = Just $ mkNode 2 Turn
  uGlobal "weld"         = Just $ mkNode 2 Weld

  uGlobal "add-assoc"    = Just $ mkNode 5 AddAssoc
  uGlobal "find-assoc"   = Just $ mkNode 3 FindAssoc

  uGlobal _              = Nothing


-- Useful Types ----------------------------------------------------------------

type Nat = Natural
type Bol = Bool


-- Raw Uruk (Basically just used for D (jam)) ----------------------------------

data Pri = S | K | E | W
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
  Ess   -> Raw S []
  Kay   -> Raw K []
  Enh 1 -> Raw E []
  Dub   -> Raw W []
  n     -> error ("TODO: nodeRaw." <> show n)

priFun :: Pri -> (Int, Node)
priFun = \case
  S -> (3, Ess)
  K -> (2, Kay)
  E -> (1, Enh 1)
  W -> (6, Dub)

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
  let jet = (Opt.regOpt . Opt.inline . Opt.optToFast) cod

  putStrLn ("  args: " <> tshow args)
  putStrLn ("  name: " <> tshow jet)

  putStrLn ("  body:")
  putStrLn (indent $ pack $ ppShow body)

  putStrLn "  code:"
  putStrLn (indent (pack $ ppShow cod))

  putStrLn ("\nJET: " <> tshow jet <> ":")
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
    Enh n -> case x of
      VFun (Fun 2 (Enh 1) _) -> pure (VFun (Fun 1 (Enh (n + 1)) (mkClo1 y)))
      _                      -> jetRegister n x y

    Dub       -> dub (v 0) (v 1) (v 2) (v 3) (v 4) (v 5)

    Add       -> add x y
    Mul       -> mul x y
    Lsh       -> dLsh x y

    Lth       -> dLth x y
    Sub       -> sub x y
    Fub       -> dFub x y
    Not       -> dNot x
    Xor       -> dXor x y
    Div       -> dDiv x y
    Tra       -> dTra x
    Mod       -> dMod x y
    Rap       -> dRap x y
    Zing      -> dZing x
    Ntot      -> dNtot x

    Lis l -> dLis l x y

    IntPositive -> dIntPositive x
    IntNegative -> dIntNegative x

    IntAbs -> dIntAbs x
    IntAdd -> dIntAdd x y
    IntDiv -> dIntDiv x y
    IntIsZer -> dIntIsZer x
    IntIsNeg -> dIntIsNeg x
    IntIsPos -> dIntIsPos x
    IntLth -> dIntLth x y
    IntMul -> dIntMul x y
    IntNegate -> dIntNegate x
    IntSub -> dIntSub x y

    MkBox     -> pure (VBox x)
    Box x     -> pure x
    Unbox     -> dUnbox x

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
    Cas       -> dCas x y z
    Let       -> dLet x y
    Nat n     -> dNat n x y
    Int i     -> dInt i x

    LCon      -> dLCon x y
    LNil      -> pure (VLis [])
    Gulf      -> dGulf x y
    Snag      -> dSnag x y
    Turn      -> dTurn x y
    Weld      -> dWeld x y

    AddAssoc  -> dAddAssoc x y z (v 3) (v 4)
    FindAssoc -> dFindAssoc x y z

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

execJet1 :: Jet -> Val -> IO Val
execJet1 !j !x = do
  -- traceCall j [x] False
  (reg, setReg) <- mkRegs (jRegs j)
  let args = fromList [x]
  let refr = \case
        0 -> pure x
        n -> throwIO (BadRef j n)
  withFallback j args (execJetBody j refr reg setReg)

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

execJet3 :: Jet -> Val -> Val -> Val -> IO Val
execJet3 !j !x !y !z = do
  -- traceCall j [x,y,z] False
  (reg, setReg) <- mkRegs (jRegs j)
  let args = fromList [x, y, z]
  let refr = \case
        0 -> pure x
        1 -> pure y
        2 -> pure z
        n -> throwIO (BadRef j n)
  withFallback j args (execJetBody j refr reg setReg)

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

execJet5 :: Jet -> Val -> Val -> Val -> Val -> Val -> IO Val
execJet5 !j !x !y !z !p !q = do
  -- traceCall j [x,y,z,p,q] False
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

execJetN :: Jet -> CloN -> IO Val
execJetN !j !xs = do
  -- traceCall j (toList xs) (jRegs j /= 0)
  (reg, setReg) <- mkRegs (jRegs j)
  let refr = pure . indexSmallArray xs
  withFallback j xs (execJetBody j refr reg setReg)


-- Self-Calls (No Tracing) -----------------------------------------------------

recJet1 :: Jet -> Val -> IO Val
recJet1 !j !x = do
  (reg, setReg) <- mkRegs (jRegs j)
  let refr = \case
        0 -> pure x
        n -> throwIO (BadRef j n)
  execJetBody j refr reg setReg

recJet2 :: Jet -> Val -> Val -> IO Val
recJet2 !j !x !y = do
  (reg, setReg) <- mkRegs (jRegs j)
  let refr = \case
        0 -> pure x
        1 -> pure y
        n -> throwIO (BadRef j n)
  execJetBody j refr reg setReg

recJet3 :: Jet -> Val -> Val -> Val -> IO Val
recJet3 !j !x !y !z = do
  (reg, setReg) <- mkRegs (jRegs j)
  let refr = \case
        0 -> pure x
        1 -> pure y
        2 -> pure z
        n -> throwIO (BadRef j n)
  execJetBody j refr reg setReg

recJet4 :: Jet -> Val -> Val -> Val -> Val -> IO Val
recJet4 !j !x !y !z !p = do
  (reg, setReg) <- mkRegs (jRegs j)
  let refr = \case
        0 -> pure x
        1 -> pure y
        2 -> pure z
        3 -> pure p
        n -> throwIO (BadRef j n)
  execJetBody j refr reg setReg

recJet5 :: Jet -> Val -> Val -> Val -> Val -> Val -> IO Val
recJet5 !j !x !y !z !p !q = do
  (reg, setReg) <- mkRegs (jRegs j)
  let refr = \case
        0 -> pure x
        1 -> pure y
        2 -> pure z
        3 -> pure p
        4 -> pure q
        n -> throwIO (BadRef j n)
  execJetBody j refr reg setReg

recJetN :: Jet -> CloN -> IO Val
recJetN !j !xs = do
  (reg, setReg) <- mkRegs (jRegs j)
  let refr = pure . indexSmallArray xs
  execJetBody j refr reg setReg

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

dLet :: Val -> Val -> IO Val
{-# INLINE dLet #-}
dLet x k = kVV k x

dCas :: Val -> Val -> Val -> IO Val
{-# INLINE dCas #-}
dCas (VLef x) l r = kVV l x
dCas (VRit x) l r = kVV r x
dCas (VLis (x:xs)) l r = kVV l (VCon x (VLis xs))
dCas (VLis [])     l r = kVV r VUni               -- technically could happen
dCas (VFun (Fun 2 LNil mempty)) l r = kVV r VUni -- the main loop termination
dCas (VFun (Fun 2 (Lis []) mempty)) l r = kVV r VUni -- the main loop termination
dCas c        _ _ = do
  pPrint c
  throwIO (TypeError "cas-not-sum: ")

dAddAssoc :: Val -> Val -> Val -> Val -> Val -> IO Val
dAddAssoc lt eq (VLis ((VCon curKey curVal):xs)) k v =
  kVVV eq curKey k >>= \case
    VBol True -> pure $ VLis ((VCon k v):xs)
    VBol False -> kVVV lt curKey k >>= \case
      VBol True -> do
        dAddAssoc lt eq (VLis xs) k v >>= \case
          (VLis recursed) -> pure $ VLis ((VCon curKey curVal):recursed)
          _ -> throw (TypeError ("impossible-in-dass-assoc"))
      VBol False -> do
        pure $ VLis ((VCon k v):(VCon curKey curVal):xs)
      _ -> throwIO (TypeError ("add-assoc-lt-not-bool"))
    _ -> throwIO (TypeError ("add-assoc-eq-not-bool"))
dAddAssoc _ _ (VLis []) k v = pure $ VLis [VCon k v]
dAddAssoc _ _ (VFun (Fun 2 LNil mempty)) k v = pure $ VLis [VCon k v]
dAddAssoc _ _ (VFun (Fun 2 (Lis []) mempty)) k v = pure $ VLis [VCon k v]
dAddAssoc _ _ _ _ _ = throwIO (TypeError ("dAddAssoc type error somewhere"))

dFindAssoc :: Val -> Val -> Val -> IO Val
dFindAssoc eq (VLis ((VCon curKey curVal):xs)) k =
  kVVV eq curKey k >>= \case
    VBol True -> pure $ VLef curVal
    VBol False -> dFindAssoc eq (VLis xs) k
    _ -> throw (TypeError "find-assoc-eq-not-bool")
dFindAssoc _ (VLis []) _ = pure $ VRit VUni
dFindAssoc _ (VFun (Fun 2 LNil mempty)) _ = pure $ VRit VUni
dFindAssoc _ (VFun (Fun 2 (Lis []) mempty)) _ = pure $ VRit VUni
dFindAssoc _ _ _ = throwIO (TypeError "dFindAssoc type error somewhere")

seq :: Val -> Val -> IO Val
{-# INLINE seq #-}
seq x y = pure y

dNat :: Natural -> Val -> Val -> IO Val
{-# INLINE dNat #-}
dNat n inc zer = go n
 where
  go = \case
    0 -> pure zer
    n -> kVA inc (go (n-1))

-- TODO Verify
dLis :: [Val] -> Val -> Val -> IO Val
dLis []     n c = kVV n VUni
dLis (x:xs) n c = kVVV c x (VLis xs)

dInt :: Integer -> Val -> IO Val
{-# INLINE dInt #-}
dInt i f | i >= 0 = kVVV f (VBol True) (VNat (fromIntegral i))
dInt i f = kVVV f (VBol False) (VNat (fromIntegral $ negate i))

dLCon :: Val -> Val -> IO Val
{-# INLINE dLCon #-}
dLCon x (VLis rest)           = pure (VLis (x:rest))
dLCon x (VFun (Fun 2 LNil _)) = pure (VLis [x])
dLCon x (VFun (Fun 2 (Lis l) xs)) | null (toList xs) = pure (VLis (x:l))
dLCon x y                     = do
  print ("LCON", x, y)
  throwIO (TypeError "dlcon-not-list")

dGulf :: Val -> Val -> IO Val
{-# INLINE dGulf #-}
dGulf (VNat x) (VNat y) = pure $ VLis [VNat n | n <- [x..y]]
dGulf _ _               = throwIO (TypeError "dGulf-not-nats")


dSnag :: Val -> Val -> IO Val
{-# INLINE dSnag #-}
dSnag (VNat n) (VLis (x:xs)) | n > 0 = dSnag (VNat (n - 1)) (VLis xs)
dSnag (VNat 0) (VLis (x:xs)) = pure x
dSnag (VNat _) (VLis []) = throwIO (TypeError "snag-fail")  -- TODO: "Crash"
dSnag idx lst            =
  throwIO (TypeError ("snag-bag-args: " ++ (tshow idx) ++ ", " ++ (tshow lst)))

dTurn :: Val -> Val -> IO Val
{-# INLINE dTurn #-}
dTurn (VLis x) fun = VLis <$> mapM (kVV fun) x
dTurn y _          = throwIO (TypeError ("turn-not-list: " ++ (tshow y)))

dWeld :: Val -> Val -> IO Val
{-# INLINE dWeld #-}
dWeld (VLis x             ) (VLis y             ) = pure $ VLis (x ++ y)
-- TODO: Make the nil case handling not so ugly.
dWeld (VFun (Fun 2 LNil _)) (VFun (Fun 2 LNil _)) = pure $ VLis []
dWeld (VLis x             ) (VFun (Fun 2 LNil _)) = pure $ VLis x
dWeld (VFun (Fun 2 LNil _)) (VLis y             ) = pure $ VLis y
dWeld a                     b                     = throwIO
  (TypeError ("dWeld-not-lists: a=" ++ (tshow a) ++ ", b=" ++ (tshow b)))

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

dub :: Val -> Val -> Val -> Val -> Val -> Val -> IO Val
{-# INLINE dub #-}
-- TODO: How do I implement apply here? The stack has been resolved entirely
dub a s k e w val = throwIO (TypeError "can't check dubs yet")

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

dRap :: Val -> Val -> IO Val
{-# INLINE dRap #-}
dRap (VNat 8) y = dCrip y
dRap (VNat _) y = throwIO (TypeError "only (rap 8) is jetted.")
dRap _        _ = throwIO (TypeError "rap-not-nat")

dIntPositive :: Val -> IO Val
{-# INLINE dIntPositive #-}
dIntPositive (VNat n) = pure $ VInt $ fromIntegral n
dIntPositive _        = throwIO $ TypeError "int-positive-not-nat"

dIntNegative :: Val -> IO Val
{-# INLINE dIntNegative #-}
dIntNegative (VNat n) = pure $ VInt $ negate $ fromIntegral n
dIntNegative _        = throwIO $ TypeError "int-positive-not-nat"

dIntAbs :: Val -> IO Val
{-# INLINE dIntAbs #-}
dIntAbs (VInt i) = pure (VInt (abs i))
dIntAbs x        = throwIO $ TypeError "int-abs: not int"

dIntAdd :: Val -> Val -> IO Val
{-# INLINE dIntAdd #-}
dIntAdd (VInt x) (VInt y) = pure $ VInt (x + y)
dIntAdd _        _        = throwIO $ TypeError "int-add-not-int"

dIntDiv :: Val -> Val -> IO Val
{-# INLINE dIntDiv #-}
dIntDiv (VInt x) (VInt y) = pure $ VInt (x `div` y)
dIntDiv _        _        = throwIO $ TypeError "int-div-not-int"

dIntIsZer :: Val -> IO Val
{-# INLINE dIntIsZer #-}
dIntIsZer (VInt i) = pure $ VBol (i == 0)
dIntIsZer _        = throwIO $ TypeError "int-is-zer-not-int"

dIntIsNeg :: Val -> IO Val
{-# INLINE dIntIsNeg #-}
dIntIsNeg (VInt i) = pure $ VBol (i < 0)
dIntIsNeg _        = throwIO $ TypeError "int-is-neg-not-int"

dIntIsPos :: Val -> IO Val
{-# INLINE dIntIsPos #-}
dIntIsPos (VInt i) = pure $ VBol (i > 0)
dIntIsPos _        = throwIO $ TypeError "int-is-pos-not-int"

dIntLth :: Val -> Val -> IO Val
{-# INLINE dIntLth #-}
dIntLth (VInt x) (VInt y) = pure $ VBol (x < y)
dIntLth _        _        = throwIO $ TypeError "int-lth-not-int"

dIntMul :: Val -> Val -> IO Val
{-# INLINE dIntMul #-}
dIntMul (VInt x) (VInt y) = pure $ VInt (x * y)
dIntMul x        y        = do
  print ("int-mu", x, y)
  throwIO $ TypeError "int-mul-not-int"

dIntNegate :: Val -> IO Val
{-# INLINE dIntNegate #-}
dIntNegate (VInt x) = pure $ VInt (negate x)
dIntNegate _        = throwIO $ TypeError "int-negate-not-int"

dIntSub :: Val -> Val -> IO Val
{-# INLINE dIntSub #-}
dIntSub (VInt x) (VInt y) = pure $ VInt (x - y)
dIntSub _        _        = throwIO $ TypeError "int-sub-not-int"

dUnbox :: Val -> IO Val
{-# INLINE dUnbox #-}
dUnbox (VBox x) = pure x
dUnbox (VFun (Fun 1 (Box x) _)) = pure $ x
dUnbox x        = do
  print x
  throwIO $ TypeError "unbox-not-box"

listToVal :: [Val] -> Val
{-# INLINE listToVal #-}
listToVal []    = VRit VUni
listToVal (h:t) = VLef (VCon h (listToVal t))

dZing :: Val -> IO Val
{-# INLINE dZing #-}
dZing = evaluate . VLis . mconcat . parseLists
 where
  parseLists :: Val -> [[Val]]
  parseLists = fmap parseList . parseList

  parseList :: Val -> [Val]
  parseList (VLis x) = x
  parseList _        = throw (TypeError "zing-not-list")

dNtot :: Val -> IO Val
{-# INLINE dNtot #-}
dNtot (VNat n) =
  evaluate $ force $ VLis $ fmap (VNat . fromIntegral . C.ord) $ show n
dNtot n = throwIO (TypeError "ntot-not-nat")

dCrip :: Val -> IO Val
{-# INLINE dCrip #-}
dCrip = evaluate . VNat . Atom.bytesAtom . BS.pack . parseTape
 where
  parseTape :: Val -> [Word8]
  parseTape (VLis []        ) = []
  parseTape (VLis (x:xs)    ) = parseByte x : parseTape (VLis xs)
  parseTape _                 = throw (TypeError "crip-not-list")

  parseByte :: Val -> Word8
  parseByte (VNat h) | h >= 256 = throw (TypeError "crip-elem-not-byte")
  parseByte (VNat h) = fromIntegral h
  parseByte _        = throw (TypeError "crip-elem-not-nat")

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

dTra :: Val -> IO Val
{-# INLINE dTra #-}
dTra x = do
  putStrLn ("TRACE: " <> tshow x)
  pure VUni

sub :: Val -> Val -> IO Val
{-# INLINE sub #-}
sub (VNat x) (VNat y) | y > x = pure (VLef VUni)
sub (VNat x) (VNat y) = pure (VRit (VNat (x - y)))
sub _        _        = throwIO (TypeError "sub-not-nat")

dFub :: Val -> Val -> IO Val
{-# INLINE dFub #-}
dFub (VNat x) (VNat y) | y > x = pure (VNat 0)
dFub (VNat x) (VNat y) = pure (VNat (x - y))
dFub _        _        = throwIO (TypeError "fub-not-nat")

zer :: Val -> IO Val
{-# INLINE zer #-}
zer (VNat 0) = pure (VBol True)
zer (VNat n) = pure (VBol False)
zer v        = throwIO (TypeError ("zer-not-nat: " <> tshow v))

eql :: Val -> Val -> IO Val
{-# INLINE eql #-}
eql (VNat x) (VNat y) = pure (VBol (x == y))
eql x        y        = do
  print x
  print y
  throwIO (TypeError "eql-not-nat")

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

clo1 :: Fun -> Val -> Val
{-# INLINE clo1 #-}
clo1 f x = cloN f (fromList [x])

clo2 :: Fun -> Val -> Val -> Val
{-# INLINE clo2 #-}
clo2 f x y = cloN f (fromList [x,y])

clo3 :: Fun -> Val -> Val -> Val -> Val
{-# INLINE clo3 #-}
clo3 f x y z = cloN f (fromList [x,y,z])

clo4 :: Fun -> Val -> Val -> Val -> Val -> Val
{-# INLINE clo4 #-}
clo4 f x y z p = cloN f (fromList [x,y,z,p])

clo5 :: Fun -> Val -> Val -> Val -> Val -> Val -> Val
{-# INLINE clo5 #-}
clo5 f x y z p q = cloN f (fromList [x,y,z,p,q])

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
    REC1  x         -> join (recJet1 j <$> go x)
    REC2  x y       -> join (recJet2 j <$> go x <*> go y)
    REC3  x y z     -> join (recJet3 j <$> go x <*> go y <*> go z)
    REC4  x y z p   -> join (recJet4 j <$> go x <*> go y <*> go z <*> go p)
    REC5  x y z p q -> join (recJet5 j <$> go x <*> go y <*> go z <*> go p <*> go q)
    RECN  xs        -> join (recJetN j <$> traverse go xs)
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
    TRA  x          -> join (dTra <$> go x)
    MOD  x y        -> join (dMod <$> go x <*> go y)
    RAP  x y        -> join (dRap <$> go x <*> go y)
    GULF x y        -> join (dGulf <$> go x <*> go y)
    SNAG x y        -> join (dSnag <$> go x <*> go y)
    TURN x y        -> join (dTurn <$> go x <*> go y)
    WELD x y        -> join (dWeld <$> go x <*> go y)
    ZING x          -> join (dZing <$> go x)
    NTOT x          -> join (dNtot <$> go x)

    -- TODO: It looks like we're going down the slow paths for these in reduce
    -- even though we have definitions in OptToFast?
    ADD_ASSOC x y z p q -> join (dAddAssoc <$> go x <*> go y <*> go z <*>
                                 go p <*> go q)
    FIND_ASSOC x y z -> join (dFindAssoc <$> go x <*> go y <*> go z)

    INT_POSITIVE x -> join (dIntPositive <$> go x)
    INT_NEGATIVE x -> join (dIntNegative <$> go x)

    INT_ABS x -> join (dIntAbs <$> go x)
    INT_ADD x y -> join (dIntAdd <$> go x <*> go y)
    INT_DIV x y -> join (dIntDiv <$> go x <*> go y)
    INT_IS_ZER x -> join (dIntIsZer <$> go x)
    INT_IS_NEG x -> join (dIntIsNeg <$> go x)
    INT_IS_POS x -> join (dIntIsPos <$> go x)
    INT_LTH x y -> join (dIntLth <$> go x <*> go y)
    INT_MUL x y -> join (dIntMul <$> go x <*> go y)
    INT_NEGATE x -> join (dIntNegate <$> go x)
    INT_SUB x y -> join (dIntSub <$> go x <*> go y)

    BOX x           -> VBox <$> go x
    UNBOX x         -> join (dUnbox <$> go x)

    SUB  x y        -> join (sub <$> go x <*> go y)
    ZER x           -> join (zer <$> go x)
    EQL x y         -> join (eql <$> go x <*> go y)
    CAR x           -> join (car <$> go x)
    CDR x           -> join (cdr <$> go x)

    CLO1 f x         -> clo1 f <$> go x
    CLO2 f x y       -> clo2 f <$> go x <*> go y
    CLO3 f x y z     -> clo3 f <$> go x <*> go y <*> go z
    CLO4 f x y z p   -> clo4 f <$> go x <*> go y <*> go z <*> go p
    CLO5 f x y z p q -> clo5 f <$> go x <*> go y <*> go z <*> go p <*> go q
    CLON f xs        -> cloN f <$> traverse go xs

    CALN f xs       -> do { f <- go f; kVAn f (go <$> xs) }
    LEF x           -> VLef <$> go x
    RIT x           -> VRit <$> go x
    CON x y         -> VCon <$> go x <*> go y
    LNIL            -> pure (VLis [])
    LCON x y        -> join (dLCon <$> go x <*> go y)
    IFF c t e       -> go c >>= \case
      VBol True  -> go t
      VBol False -> go e
      cv         -> do
        print ("iff", cv, t, e)
        print ("iff.cond", c)
        throwIO (TypeError "iff-not-bol")
    LET i x k  -> (go x >>= setReg i) >> go k
    THE x k  -> go x >> go k

    FOR i lisExp bod -> do
      -- putStrLn "FOR"
      -- putStrLn ("  reg: " <> tshow i)
      -- putStrLn ("  bod: " <> tshow bod)
      lis <- go lisExp
      -- putStrLn ("  lisval: " <> tshow lis)
      xs  <- case lis of
        VLis xs -> pure xs
        _       -> throwIO (TypeError "turn-not-list")
      res <- for xs $ \xv -> do
        -- putStrLn ("  liselm:" <> tshow xv)
        setReg i xv
        go bod
      -- putStrLn ("  forres:" <> tshow res)
      pure (VLis res)

    CAS i x l r -> go x >>= \case
      VLef lv -> setReg i lv >> go l
      VRit rv -> setReg i rv >> go r
      -- TODO: Move from con cell construction to curry so |=((h t) ...) works?
      VLis (x:xs) -> setReg i (VCon x (VLis xs)) >> go l
      VLis []     -> setReg i VUni >> go r
      VFun (Fun 2 LNil mempty) -> setReg i VUni >> go r
      VFun (Fun 2 (Lis []) mempty) -> setReg i VUni >> go r
      _       -> throwIO (TypeError "cas-not-sum")

-- Profiling -------------------------------------------------------------------

vProfQ :: IORef [Event]
vProfQ = unsafePerformIO (newIORef [])

vProfDone :: TVar Bool
vProfDone = unsafePerformIO (newTVarIO False)

toDisk :: Event -> EventDisk
toDisk (Event star time jett) =
  EventDisk
    star
    (round (1000000 * time) :: Word64)
    (jetTagFast (jName jett))

{-# INLINE jetTagFast #-}
jetTagFast :: Val -> ByteString
jetTagFast = \case
  VNat n -> Atom.atomBytes n
  _      -> error "TODO"

dumpEventsFile :: FilePath -> IO ()
dumpEventsFile fp = do
  Sys.withFile fp Sys.WriteMode $ \h -> do
    hSetBuffering h (BlockBuffering (Just 1_000_000))
    dumpEvents h
    hFlush h

{-# INLINE dumpEvents #-}
dumpEvents :: Handle -> IO ()
dumpEvents h = do
  tid <- async go
  void (wait tid)
 where
  go = do
    done <- atomically (readTVar vProfDone)
    evs  <- atomicModifyIORef vProfQ (\es -> ([], es))
    let numevs = length evs
    unless (null evs) $ do
      -- putStrLn ("{" <> tshow numevs <> "}")
      BS.hPut h $ Store.encode $ toDisk <$> evs
    unless (null evs && done) $ do
      when (numevs < 333) $ do
        threadDelay 100_000
      go

{-# INLINE jetOkToTrace #-}
jetOkToTrace :: Jet -> Bool
jetOkToTrace j = case jName j of
  VNat 123584151057773                                -> False -- mul-fp
  VNat 135882211239269503500120678                    -> False -- fraction-fp
  VNat 1651864435                                     -> False -- ssub
  VNat 1684825463                                     -> False -- weld
  VNat 1752460403                                     -> False -- slth
  VNat 1819635059                                     -> False -- smul
  VNat 1836413811                                     -> False -- ssum
  VNat 1907323525685090546547                         -> False -- ssum-aneg
  VNat 1935827315                                     -> False -- sabs
  VNat 1936617315                                     -> False -- cons
  VNat 1952542323                                     -> False -- snat
  VNat 1953461358                                     -> False -- ntot
  VNat 1986618483                                     -> False -- sdiv
  VNat 2003136115                                     -> False -- snew
  VNat 2074065091858577454190                         -> False -- ntot-loop
  VNat 2129405593459937866611                         -> False -- ssub-apos
  VNat 2551989185581783952010596030736099647343583587 -> False -- calculate-color-for
  VNat 29099049607852661                              -> False -- ur-snag
  VNat 1734438515                                     -> False -- snag
  VNat 474181366643                                   -> False -- ssign
  VNat 482750590836                                   -> False -- to-fp
  VNat 7627107                                        -> False -- cat
  _                                                   -> True

{-# INLINE traceCall #-}
traceCall :: Jet -> [Val] -> Bool -> IO ()
traceCall j xs reg = do
  -- print ("KAL", j)
  when (jetOkToTrace j) $ do
    t <- getPOSIXTime
    atomicModifyIORef vProfQ (\es -> (Event True t j:es, ()))

{-# INLINE traceResu #-}
traceResu :: Jet -> [Val] -> Val -> IO ()
traceResu j xs val = do
  -- print ("END", j)
  when (jetOkToTrace j) $ do
    t <- getPOSIXTime
    atomicModifyIORef vProfQ (\es -> (Event False t j:es, ()))


--------------------------------------------------------------------------------

loadProfLog :: FilePath -> IO [EventDisk]
loadProfLog fp = do
  bs <- readFile fp
  go [] bs
 where
  go :: [EventDisk] -> ByteString -> IO [EventDisk]
  go acc bs | null bs = pure acc
  go acc bs = do
    (off, val) <- Store.decodeIOPortionWith Store.peek bs
    let acc' = acc <> reverse val
    go acc' (BS.drop off bs)

toJSON :: FilePath -> FilePath -> IO ()
toJSON binFP jsoFP = do
  evs <- loadProfLog binFP
  Sys.withFile jsoFP Sys.WriteMode $ \jh -> do
    case evs of
      []   -> pure ()
      e:es -> do
        BS.hPut jh "[ "
        eventJson jh e
        for_ evs $ \ev -> do
          BS.hPut jh ", "
          eventJson jh ev

eventJson :: Handle -> EventDisk -> IO ()
eventJson h (EventDisk True tim nam) = do
  BS.hPut h
    "{\"ph\":\"B\",\"cat\":\"j\",\"pid\":1,\"tid\":1,\"args\":{},\"ts\":"
  Sys.hPutStr h (show tim)
  BS.hPut h ",\"name\":\""
  BS.hPut h nam
  BS.hPut h "\"}\n"
eventJson h (EventDisk False tim nam) = do
  Sys.hPutStr h
    "{\"ph\":\"E\",\"cat\":\"j\",\"pid\":1,\"tid\":1,\"args\":{},\"ts\":"
  Sys.hPutStr h (show tim)
  Sys.hPutStr h ",\"name\":\""
  BS.hPut h nam
  Sys.hPutStr h "\"}\n"
