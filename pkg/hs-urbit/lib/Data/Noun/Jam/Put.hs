{-# LANGUAGE MagicHash #-}

module Data.Noun.Jam.Put where

import ClassyPrelude
import GHC.Word (Word(W#))
import GHC.Int (Int(I#))
import GHC.Prim
import GHC.Natural
import GHC.Integer.GMP.Internals
import Data.Vector.Primitive ((!))

import Control.Lens          (view)
import Control.Monad         (guard)
import Data.Bits             (shiftL, shiftR, setBit, clearBit, (.|.), (.&.))
import Data.Map              (Map)
import Data.Noun.Atom        (Atom(MkAtom), wordBitWidth#)
import Data.Noun             (Noun(Atom, Cell))
import Data.Noun.Pill        (bigNatWords)
import Data.Noun.Atom        (toAtom, takeBits, bitWidth)
import Foreign.Marshal.Alloc (mallocBytes, free)
import Foreign.Ptr           (Ptr, castPtr, plusPtr, ptrToWordPtr)
import Foreign.Storable      (peek, poke)
import System.IO.Unsafe      (unsafePerformIO)

import qualified Data.ByteString.Unsafe as BS
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
  { runPut :: H.LinearHashTable Word Noun
           -> S
           -> IO (PutResult a)
  }

--------------------------------------------------------------------------------

{-
  1. Write the register to the output, and increment the output pointer.
-}
flush :: Put ()
flush = Put $ \tbl s@S{..} -> do
    poke ptr reg
    pure $ PutResult (s { ptr = ptr `plusPtr` 8 }) ()
{-# INLINE flush #-}

update :: (S -> S) -> Put ()
update f = Put \tbl s@S{..} -> pure (PutResult (f s) ())
{-# INLINE update #-}

setRegOff :: Word -> Int -> Put ()
setRegOff r o = update \s@S{..} -> (s {reg=r, off=o})
{-# INLINE setRegOff #-}

setReg :: Word -> Put ()
setReg r = update \s@S{..} -> (s { reg=r })
{-# INLINE setReg #-}

getS :: Put S
getS = Put $ \tbl s -> pure (PutResult s s)
{-# INLINE getS #-}

putS :: S -> Put ()
putS s = Put $ \tbl _ -> pure (PutResult s ())
{-# INLINE putS #-}

{-
    To write a bit:

    | reg  |= 1 << regI
    | regI <- (regI + 1) % 64
    | if (!regI):
    |     buf[w++] <- reg
    |     reg      <- 0
-}
writeBit :: Bool -> Put ()
writeBit b = Put $ \tbl s@S{..} -> do
  let s' = s { reg = (if b then setBit else clearBit) reg off
             , off = (off + 1) `mod` 64
             , pos = pos + 1
             }

  if off == 63
  then runPut (flush >> setRegOff 0 0) tbl s'
  else pure $ PutResult s' ()
{-# INLINE writeBit #-}

{-
    To write a 64bit word:

    | reg |= w << regI
    | buf[bufI++] = reg
    | reg = w >> (64 - regI)
-}
writeWord :: Word -> Put ()
writeWord wor = do
    S{..} <- getS
    setReg (reg .|. shiftL wor off)
    flush
    setReg (shiftR wor (64 - off))
{-# INLINE writeWord #-}

{-
    To write some bits (< 64) from a word:

    | reg  |= wor << regI
    | regI += wid
    |
    | if (regI >= 64)
    |     regI -= 64
    |     buf[w] = x
    |     reg = wor >> (wid - regI)
-}
writeBitsFromWord :: Int -> Word -> Put ()
writeBitsFromWord wid wor = do
    s <- getS

    let s' = s { reg = reg s .|. shiftL wor (off s)
               , off = off s + wid
               }

    if (off s' < 64)
    then do putS s'
    else do update (\s -> s { off = off s - 64 })
            flush
            setReg (shiftR wor (wid - off s'))
{-
  Write all of the the signficant bits of a direct atom.
-}
writeAtomWord# :: Word# -> Put ()
writeAtomWord# w = writeBitsFromWord (I# (word2Int# (wordBitWidth# w))) (W# w)

writeAtomWord :: Word -> Put ()
writeAtomWord (W# w) = writeAtomWord# w

{-
  Write all of the the signficant bits of an indirect atom.

  TODO Use memcpy when the bit-offset of the output is divisible by 8.
-}
writeAtomBigNat :: BigNat -> Put ()
writeAtomBigNat (view bigNatWords -> words) = do
  let lastIdx = VP.length words - 1
  for_ [0..(lastIdx-1)] \i ->
      writeWord (words ! i)
  writeAtomWord (words ! lastIdx)

writeAtomBits :: Atom -> Put ()
writeAtomBits = \case MkAtom (NatS# wd) -> writeAtomWord# wd
                      MkAtom (NatJ# bn) -> writeAtomBigNat bn

--------------------------------------------------------------------------------

instance Functor Put where
    fmap f g = Put $ \tbl s -> do
        PutResult s' a <- runPut g tbl s
        pure $ PutResult s' (f a)
    {-# INLINE  fmap #-}

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

doPut :: Word64 -> Put () -> ByteString
doPut sz m =
    unsafePerformIO $ do
        tbl <- H.new
        buf <- mallocBytes (fromIntegral $ wordSz*8)
        _   <- runPut m tbl (S buf 0 0 0)
        BS.unsafePackCStringFinalizer (castPtr buf) byteSz (free buf)
  where
    wordSz = fromIntegral (sz `divUp` 64)
    byteSz = fromIntegral (sz `divUp` 8)
    divUp x y = (x `div` y) + (if x `mod` y == 0 then 0 else 1)

--------------------------------------------------------------------------------

{-
    TODO Handle back references
-}
writeNoun :: Noun -> Put ()
writeNoun = \case Atom a   -> writeAtom a
                  Cell h t -> writeCell (h, t)

writeMat :: Atom -> Put ()
writeMat atm = do
    writeBitsFromWord (preWid+1) (shiftL (1 :: Word) preWid)
    writeAtomBits extras
    writeAtomBits atm
  where
    atmWid = bitWidth atm :: Atom
    preWid = bitWidth atmWid :: Int
    prefix = shiftL (1 :: Word) (fromIntegral preWid)
    extras = takeBits (preWid-1) (toAtom atmWid)

writeCell :: (Noun, Noun) -> Put ()
writeCell (h, t) = do
    writeBit True
    writeBit False
    writeNoun h
    writeNoun t

writeAtom :: Atom -> Put ()
writeAtom a = writeBit False >> writeMat a

writeBackRef :: Atom -> Put ()
writeBackRef a = do
    writeBit True
    writeBit True
    writeMat a
