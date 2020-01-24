{-# OPTIONS_GHC -O2 #-}

{-|
    Fast implementation of Jam (Noun → Atom).

    This is based on the implementation of `flat`.
-}
module Ur.Noun.Jam (jam, jamBS) where

import ClassyPrelude hiding (hash)

import Urbit.Atom
import Urbit.Atom.Internal
import Ur.Noun.Core

import Data.Bits                 (clearBit, setBit, shiftL, shiftR, (.|.))
import Data.Vector.Primitive     ((!))
import Foreign.Marshal.Alloc     (callocBytes, free)
import Foreign.Ptr               (Ptr, castPtr, plusPtr)
import Foreign.Storable          (poke)
import GHC.Int                   (Int(I#))
import GHC.Integer.GMP.Internals (BigNat)
import GHC.Natural               (Natural(NatJ#, NatS#))
import GHC.Prim                  (Word#, plusWord#, word2Int#)
import GHC.Word                  (Word(W#))
import System.IO.Unsafe          (unsafePerformIO)

import qualified Data.ByteString.Unsafe as BS
import qualified Data.HashTable.IO      as H
import qualified Data.Vector.Primitive  as VP


-- Exports ---------------------------------------------------------------------

jamBS :: Noun -> ByteString
jamBS n = doPut bt sz (writeNoun n)
  where
    (sz, bt) = unsafePerformIO (compress n)

jam :: Noun -> Atom
jam = bytesAtom . jamBS


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
  { runPut :: H.CuckooHashTable Word Word
           -> S
           -> IO (PutResult a)
  }

--------------------------------------------------------------------------------

{-# INLINE getRef #-}
getRef :: Put (Maybe Word)
getRef = Put $ \tbl s -> PutResult s <$> H.lookup tbl (pos s)

{-|
  1. Write the register to the output, and increment the output pointer.
-}
{-# INLINE flush #-}
flush :: Put ()
flush = Put $ \tbl s@S{..} -> do
    poke ptr reg
    pure $ PutResult (s { ptr = ptr `plusPtr` 8 }) ()

{-# INLINE update #-}
update :: (S -> S) -> Put ()
update f = Put $ \tbl s@S{..} -> pure (PutResult (f s) ())

{-# INLINE setRegOff #-}
setRegOff :: Word -> Int -> Put ()
setRegOff r o = update $ \s@S{..} -> (s {reg=r, off=o})

{-# INLINE setReg #-}
setReg :: Word -> Put ()
setReg r = update $ \s@S{..} -> (s { reg=r })

{-# INLINE getS #-}
getS :: Put S
getS = Put $ \tbl s -> pure (PutResult s s)

{-# INLINE putS #-}
putS :: S -> Put ()
putS s = Put $ \tbl _ -> pure (PutResult s ())

{-|
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

{-|
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
    update $ \s -> s { pos = 64 + pos
                     , reg = shiftR wor (64 - off)
                     }

{-|
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

{-|
    Write all of the the signficant bits of a direct atom.
-}
{-# INLINE writeAtomWord# #-}
writeAtomWord# :: Word# -> Put ()
writeAtomWord# w = do
    writeBitsFromWord (I# (word2Int# (wordBitWidth# w))) (W# w)

{-# INLINE writeAtomWord #-}
writeAtomWord :: Word -> Put ()
writeAtomWord (W# w) = writeAtomWord# w

{-|
  Write all of the the signficant bits of an indirect atom.

  TODO Use memcpy when the bit-offset of the output is divisible by 8.
-}
{-# INLINE writeAtomBigNat #-}
writeAtomBigNat :: BigNat -> Put ()
writeAtomBigNat !(bigNatWords -> words) = do
  let lastIdx = VP.length words - 1
  for_ [0..(lastIdx-1)] $ \i ->
      writeWord (words ! i)
  writeAtomWord (words ! lastIdx)

{-# INLINE writeAtomBits #-}
writeAtomBits :: Atom -> Put ()
writeAtomBits = \case NatS# wd -> writeAtomWord# wd
                      NatJ# bn -> writeAtomBigNat bn


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

doPut :: H.CuckooHashTable Word Word -> Word -> Put () -> ByteString
doPut !tbl !sz m =
    unsafePerformIO $ do
        -- traceM "doPut"
        buf <- callocBytes (fromIntegral (wordSz*8))
        _   <- runPut (m >> mbFlush) tbl (S buf 0 0 0)
        BS.unsafePackCStringFinalizer (castPtr buf) byteSz (free buf)
  where
    !wordSz = fromIntegral (sz `divUp` 64)
    !byteSz = fromIntegral (sz `divUp` 8)
    !divUp  = \x y -> (x `div` y) + (if x `mod` y == 0 then 0 else 1)

    mbFlush :: Put ()
    mbFlush = do
      shouldFlush <- (/= 0) . off <$> getS
      when shouldFlush flush


--------------------------------------------------------------------------------

{-|
    TODO Handle back references
-}
writeNoun :: Noun -> Put ()
writeNoun !n =
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
writeCell !h !t = do
    writeBit True
    writeBit False
    writeNoun h
    writeNoun t

{-# INLINE writeAtom #-}
writeAtom :: Atom -> Put ()
writeAtom !a = do
    writeBit False
    writeMat a

{-# INLINE writeBackRef #-}
writeBackRef :: Word -> Put ()
writeBackRef !a = do
    p <- pos <$> getS
    writeBit True
    writeBit True
    writeMat (fromIntegral a)


-- Calculate Jam Size and Backrefs ---------------------------------------------

{-# INLINE matSz #-}
matSz :: Atom -> Word
matSz !a = W# (matSz# a)

{-# INLINE matSz# #-}
matSz# :: Atom -> Word#
matSz# 0 = 1##
matSz# a = preW `plusWord#` preW `plusWord#` atmW
  where
    atmW = atomBitWidth# a
    preW = wordBitWidth# atmW

{-# INLINE atomSz #-}
atomSz :: Atom -> Word
atomSz !w = 1 + matSz w

{-# INLINE refSz #-}
refSz :: Word -> Word
refSz !w = 1 + jamWordSz w

{-# INLINE jamWordSz #-}
jamWordSz :: Word -> Word
jamWordSz 0      = 2
jamWordSz (W# w) = 1 + 2*(W# preW) + (W# atmW)
  where
    atmW = wordBitWidth# w
    preW = wordBitWidth# atmW

compress :: Noun -> IO (Word, H.CuckooHashTable Word Word)
compress !top = do
    let sz = max 50
           $ min 10_000_000
           $ (2*) $ (10^) $ floor $ logBase 600 $ fromIntegral $ nounSize top

    nodes :: H.BasicHashTable  Noun Word <- H.newSized sz
    backs :: H.CuckooHashTable Word Word <- H.newSized sz

    let proc :: Word -> Noun -> IO Word
        proc !pos = \case
            Atom a   -> pure (atomSz a)
            Cell h t -> do !hSz <- go (pos+2) h
                           !tSz <- go (pos+2+hSz) t
                           pure (2+hSz+tSz)

        go :: Word -> Noun -> IO Word
        go !p !inp = do
            H.lookup nodes inp >>= \case
                Nothing -> do
                    H.insert nodes inp p
                    proc p inp
                Just bak -> do
                    let rs    = refSz bak
                        doRef = H.insert backs p bak $> rs
                        noRef = proc p inp
                    case inp of
                        Cell _ _                              -> doRef
                        Atom a | rs < atomSz (fromIntegral a) -> doRef
                        _                                     -> noRef

    res <- go 0 top

    pure (res, backs)
