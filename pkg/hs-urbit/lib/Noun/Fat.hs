{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fwarn-unused-binds -fwarn-unused-imports #-}

module Noun.Fat ( FatNoun(..), fatSize, fatHash
                , fatCell, fatAtom
                , toFatNoun, fromFatNoun
                , jamWordSz
                , atomSz
                ) where

import ClassyPrelude hiding (hash)

import Control.Lens              (view, to, from)
import Data.Bits                 (shiftL, shiftR, setBit, clearBit, xor, (.|.))
import Noun.Atom                 (Atom(MkAtom), toAtom, bitWidth, takeBitsWord)
import Noun.Atom                 (wordBitWidth, wordBitWidth# , atomBitWidth#)
import Noun                      (Noun(Atom, Cell))
import Noun.Pill                 (bigNatWords, atomBS)
import Data.Vector.Primitive     ((!))
import Foreign.Marshal.Alloc     (callocBytes, free)
import Foreign.Ptr               (Ptr, castPtr, plusPtr)
import Foreign.Storable          (poke)
import GHC.Integer.GMP.Internals (BigNat)
import GHC.Int                   (Int(I#))
import GHC.Natural               (Natural(NatS#, NatJ#))
import GHC.Prim                  (Word#, plusWord#, word2Int#, reallyUnsafePtrEquality#)
import GHC.Word                  (Word(W#))
import System.IO.Unsafe          (unsafePerformIO)

import qualified Data.ByteString.Unsafe as BS
import qualified Data.Hashable          as Hash
import qualified Data.HashTable.IO      as H
import qualified Data.Vector.Primitive  as VP


-- Types -----------------------------------------------------------------------

{-|
    The encoder state.

    - ptr: Pointer into the output buffer.
    - reg: Next 64 bits of output, partially written.
    - off: Number of bits already written into `reg`
    - pos: Total number of bits written.
-}
data S = S
  { ptr :: {-# UNPACK #-} !(Ptr Word)
  , reg :: {-# UNPACK #-} !Word
  , off :: {-# UNPACK #-} !Int
  , pos :: {-# UNPACK #-} !Word
  } deriving (Show,Eq,Ord)

data PutResult a = PutResult {-# UNPACK #-} !S !a
  deriving Functor

newtype Put a = Put
  { runPut :: H.LinearHashTable Word Word
           -> S
           -> IO (PutResult a)
  }

--------------------------------------------------------------------------------

{-# INLINE getRef #-}
getRef :: Put (Maybe Word)
getRef = Put \tbl s -> PutResult s <$> H.lookup tbl (pos s)

{-
  1. Write the register to the output, and increment the output pointer.
-}
{-# INLINE flush #-}
flush :: Put ()
flush = Put $ \tbl s@S{..} -> do
    poke ptr reg
    pure $ PutResult (s { ptr = ptr `plusPtr` 8 }) ()

{-# INLINE update #-}
update :: (S -> S) -> Put ()
update f = Put \tbl s@S{..} -> pure (PutResult (f s) ())

{-# INLINE setRegOff #-}
setRegOff :: Word -> Int -> Put ()
setRegOff r o = update \s@S{..} -> (s {reg=r, off=o})

{-# INLINE setReg #-}
setReg :: Word -> Put ()
setReg r = update \s@S{..} -> (s { reg=r })

{-# INLINE getS #-}
getS :: Put S
getS = Put $ \tbl s -> pure (PutResult s s)

{-# INLINE putS #-}
putS :: S -> Put ()
putS s = Put $ \tbl _ -> pure (PutResult s ())

{-
    To write a bit:

    | reg  |= 1 << off
    | off <- (off + 1) % 64
    | if (!off):
    |     buf[w++] <- reg
    |     reg      <- 0
-}
{-# INLINE writeBit #-}
writeBit :: Bool -> Put ()
writeBit b = Put $ \tbl s@S{..} -> do
    let s' = s { reg = (if b then setBit else clearBit) reg off
               , off = (off + 1) `mod` 64
               , pos = pos + 1
               }

    if off == 63
    then runPut (flush >> setRegOff 0 0) tbl s'
    else pure $ PutResult s' ()

{-
    To write a 64bit word:

    | reg |= w << off
    | buf[bufI++] = reg
    | reg = w >> (64 - off)
-}
{-# INLINE writeWord #-}
writeWord :: Word -> Put ()
writeWord wor = do
    S{..} <- getS
    setReg (reg .|. shiftL wor off)
    flush
    update \s -> s { pos = 64 + pos
                   , reg = shiftR wor (64 - off)
                   }

{-
    To write some bits (< 64) from a word:

    | wor = takeBits(wid, wor)
    | reg = reg .|. (wor << off)
    | off = (off + wid) % 64
    |
    | if (off + wid >= 64)
    |     buf[w] = x
    |     reg    = wor >> (wid - off)
-}
{-# INLINE writeBitsFromWord #-}
writeBitsFromWord :: Int -> Word -> Put ()
writeBitsFromWord wid wor = do
    wor <- pure (takeBitsWord wid wor)

    oldSt <- getS

    let newSt = oldSt { reg = reg oldSt .|. shiftL wor (off oldSt)
                      , off = (off oldSt + wid) `mod` 64
                      , pos = fromIntegral wid + pos oldSt
                      }

    putS newSt

    when (wid + off oldSt >= 64) $ do
        flush
        setReg (shiftR wor (wid - off newSt))
{-
  Write all of the the signficant bits of a direct atom.
-}
{-# INLINE writeAtomWord# #-}
writeAtomWord# :: Word# -> Put ()
writeAtomWord# w = do
    writeBitsFromWord (I# (word2Int# (wordBitWidth# w))) (W# w)

{-# INLINE writeAtomWord #-}
writeAtomWord :: Word -> Put ()
writeAtomWord (W# w) = writeAtomWord# w

{-
  Write all of the the signficant bits of an indirect atom.

  TODO Use memcpy when the bit-offset of the output is divisible by 8.
-}
{-# INLINE writeAtomBigNat #-}
writeAtomBigNat :: BigNat -> Put ()
writeAtomBigNat (view bigNatWords -> words) = do
  let lastIdx = VP.length words - 1
  for_ [0..(lastIdx-1)] \i ->
      writeWord (words ! i)
  writeAtomWord (words ! lastIdx)

{-# INLINE writeAtomBits #-}
writeAtomBits :: Atom -> Put ()
writeAtomBits = \case MkAtom (NatS# wd) -> writeAtomWord# wd
                      MkAtom (NatJ# bn) -> writeAtomBigNat bn


-- Put Instances ---------------------------------------------------------------

instance Functor Put where
    fmap f g = Put $ \tbl s -> do
        PutResult s' a <- runPut g tbl s
        pure $ PutResult s' (f a)
    {-# INLINE fmap #-}

instance Applicative Put where
    pure x = Put (\_ s -> return $ PutResult s x)
    {-# INLINE pure #-}

    Put f <*> Put g = Put $ \tbl s1 -> do
        PutResult s2 f' <- f tbl s1
        PutResult s3 g' <- g tbl s2
        return $ PutResult s3 (f' g')
    {-# INLINE (<*>) #-}

    Put f *> Put g = Put $ \tbl s1 -> do
        PutResult s2 _ <- f tbl s1
        g tbl s2
    {-# INLINE (*>) #-}

instance Monad Put where
    return = pure
    {-# INLINE return #-}

    (>>) = (*>)
    {-# INLINE (>>) #-}

    Put x >>= f = Put $ \tbl s -> do
        PutResult s' x' <- x tbl s
        runPut (f x') tbl s'
    {-# INLINE (>>=) #-}


--------------------------------------------------------------------------------

doPut :: H.LinearHashTable Word Word -> Word -> Put () -> ByteString
doPut tbl sz m =
    unsafePerformIO $ do
        traceM "doPut"
        buf <- callocBytes (fromIntegral (wordSz*8))
        _   <- runPut (m >> mbFlush) tbl (S buf 0 0 0)
        BS.unsafePackCStringFinalizer (castPtr buf) byteSz (free buf)
  where
    wordSz = fromIntegral (sz `divUp` 64)
    byteSz = fromIntegral (sz `divUp` 8)
    divUp  = \x y -> (x `div` y) + (if x `mod` y == 0 then 0 else 1)

    mbFlush :: Put ()
    mbFlush = do
      shouldFlush <- (/= 0) . off <$> getS
      when shouldFlush flush


--------------------------------------------------------------------------------

{-
    TODO Handle back references
-}
writeNoun :: Noun -> Put ()
writeNoun n =
  getRef >>= \case
    Just bk -> writeBackRef bk
    Nothing -> case n of Atom a   -> writeAtom a
                         Cell h t -> writeCell h t

{-# INLINE writeMat #-}
writeMat :: Atom -> Put ()
writeMat 0   = writeBit True
writeMat atm = do
    writeBitsFromWord (preWid+1) (shiftL 1 preWid)
    writeBitsFromWord (preWid-1) atmWid
    writeAtomBits atm
  where
    atmWid = bitWidth atm
    preWid = fromIntegral (wordBitWidth atmWid)

{-# INLINE writeCell #-}
writeCell :: Noun -> Noun -> Put ()
writeCell h t = do
    writeBit True
    writeBit False
    writeNoun h
    writeNoun t

{-# INLINE writeAtom #-}
writeAtom :: Atom -> Put ()
writeAtom a = do
    writeBit False
    writeMat a

{-# INLINE writeBackRef #-}
writeBackRef :: Word -> Put ()
writeBackRef a = do
    p <- pos <$> getS
    writeBit True
    writeBit True
    writeMat (toAtom a)


-- Compute Hashes and Jam Size (with no backrefs) ------------------------------

data FatNoun
    = FatCell {-# UNPACK #-} !Word
              {-# UNPACK #-} !Int
              !FatNoun
              !FatNoun
    | FatWord {-# UNPACK #-} !Word
    | FatAtom {-# UNPACK #-} !Word
              {-# UNPACK #-} !Int
              {-# UNPACK #-} !BigNat
  deriving (Show)

{-# INLINE fatSize #-}
fatSize :: FatNoun -> Word
fatSize = \case
  FatCell s _ _ _ -> s
  FatAtom s _ _   -> s
  FatWord w       -> atomSz (fromIntegral w)

{-# INLINE fatHash #-}
fatHash :: FatNoun -> Int
fatHash = \case
  FatCell _ h _ _ -> h
  FatAtom _ h _   -> h
  FatWord w       -> Hash.hash w

instance Hashable FatNoun where
  hash = fatHash
  {-# INLINE hash #-}
  hashWithSalt = defaultHashWithSalt
  {-# INLINE hashWithSalt #-}

instance Eq FatNoun where
  (==) x y =
    case reallyUnsafePtrEquality# x y of
      1# -> True
      0# -> case (x, y) of
              (FatWord w1,          FatWord w2         ) ->
                  w1==w2
              (FatAtom s1 x1 a1,    FatAtom s2 x2 a2   ) ->
                  s1==s2 && x1==x2 && a1==a2
              (FatCell s1 x1 h1 t1, FatCell s2 x2 h2 t2) ->
                  s1==s2 && x1==x2 && h1==h2 && t1==t2
              (_,                   _                  ) ->
                  False
  {-# INLINE (==) #-}


--------------------------------------------------------------------------------

{-# INLINE fatAtom #-}
fatAtom :: Atom -> FatNoun
fatAtom = \case
  a@(MkAtom   (NatS# w))  -> FatWord (W# w)
  a@(MkAtom n@(NatJ# bn)) -> FatAtom (atomSz a) (Hash.hash bn) bn

{-# INLINE fatCell #-}
fatCell :: FatNoun -> FatNoun -> FatNoun
fatCell h t = FatCell siz has h t
  where
    siz = 2 + fatSize h + fatSize t
    has = fatHash h `combine` fatHash t

{-# INLINE jamWordSz #-}
jamWordSz :: Word -> Word
jamWordSz 0      = 2
jamWordSz (W# w) = 1 + 2*(W# preW) + (W# atmW)
  where
    atmW = wordBitWidth# w
    preW = wordBitWidth# atmW

{-# INLINE atomSz #-}
atomSz :: Atom -> Word
atomSz = (1+) . matSz

{-# INLINE matSz #-}
matSz :: Atom -> Word
matSz a = W# (matSz# a)

{-# INLINE matSz# #-}
matSz# :: Atom -> Word#
matSz# 0 = 1##
matSz# a = preW `plusWord#` preW `plusWord#` atmW
  where
    atmW = atomBitWidth# a
    preW = wordBitWidth# atmW

{-# INLINE toFatNoun #-}
toFatNoun :: Noun -> FatNoun
toFatNoun = trace "toFatNoun" . go
  where
    go (Atom a)   = fatAtom a
    go (Cell h t) = fatCell (go h) (go t)

{-# INLINE fromFatNoun #-}
fromFatNoun :: FatNoun -> Noun
fromFatNoun = trace "fromFatNoun" . go
  where go = \case
          FatAtom _ _ a   -> Atom (MkAtom $ NatJ# a)
          FatCell _ _ h t -> Cell (go h) (go t)
          FatWord w       -> Atom (fromIntegral w)


-- Stolen from Hashable Library ------------------------------------------------

{-# INLINE combine #-}
combine :: Int -> Int -> Int
combine h1 h2 = (h1 * 16777619) `xor` h2

{-# INLINE defaultHashWithSalt #-}
defaultHashWithSalt :: Hashable a => Int -> a -> Int
defaultHashWithSalt salt x = salt `combine` Hash.hash x
