{-# LANGUAGE MagicHash #-}

module Data.Noun.Jam.Fast where

import ClassyPrelude
import Data.Noun
import Data.Noun.Atom
import Data.Noun.Poet
import Data.Bits hiding (Bits)
import Control.Lens
import Text.Printf
import GHC.Prim
import GHC.Word
import GHC.Natural
import Foreign.Ptr
import Foreign.Storable (peek)

import Data.Map      (Map)
import Control.Monad (guard)

import Test.Tasty
import Test.Tasty.TH
import qualified Test.Tasty.QuickCheck as QC
import Test.QuickCheck hiding ((.&.))

import qualified Data.HashTable.IO as H

-- Pre-Calculate the bit-width of `jam` ----------------------------------------

matSz# :: Atom -> Word#
matSz# 0 = 1##
matSz# a = preW `plusWord#` preW `plusWord#` atmW
  where
    atmW = atomBitWidth# a
    preW = wordBitWidth# atmW

refSz# :: Word# -> Word#
refSz# w = 2## `plusWord#` (matSz# (MkAtom (NatS# w)))

nounSz# :: Noun -> Word#
nounSz# (Atom a)   = 1## `plusWord#` (matSz# a)
nounSz# (Cell l r) = 2## `plusWord#` (nounSz# l) `plusWord#` (nounSz# r)

jamSz :: Noun -> Word
jamSz = fst . go 0 mempty
  where
    insertNoun :: Noun -> Word -> Map Noun Word -> Map Noun Word
    insertNoun n i tbl = lookup n tbl
                       & maybe tbl (const $ insertMap n i tbl)

    go :: Word -> Map Noun Word -> Noun -> (Word, Map Noun Word)
    go off oldTbl noun =
      let tbl = insertNoun noun off oldTbl in
      case lookup noun oldTbl of
        Nothing ->
          case noun of
            Atom atm ->
              (1 + W# (matSz# atm), tbl)
            Cell l r ->
              let (lSz, tbl) = go (2+off)     tbl l in
              let (rSz, tbl) = go (2+off+lSz) tbl r in
              (2 + lSz + rSz, tbl)
        Just (W# ref) ->
          let refSz = W# (wordBitWidth# ref) in
          case noun of
            Atom atm ->
              let worSz = W# (matSz# atm) in
              if worSz > refSz
              then (refSz, oldTbl)
              else (1 + worSz, tbl)
            Cell _ _ ->
              (refSz, oldTbl)

-- How to write a faster `cue`? ------------------------------------------------

{-|
    The decoder state.

    - An array of words (internal structure of our atoms).
    - A pointer to the word *after* the last word in the array.
    - A pointer into the current word of that array.
    - A bit-offset into that word.
-}
data S = S
  { currPtr  :: {-# UNPACK #-} !(Ptr Word)
  , usedBits :: {-# UNPACK #-} !Word
  , pos      :: {-# UNPACK #-} !Word
  } deriving (Show,Eq,Ord)

--------------------------------------------------------------------------------

type Env = (Ptr Word, S)

data DecodeException = NotEnoughSpace Env
                     | TooMuchSpace Env
                     | BadEncoding Env String
  deriving (Show, Eq, Ord)

instance Exception DecodeException

badEncoding :: Ptr Word -> S -> String -> IO a
badEncoding endPtr s msg = throwIO $ BadEncoding (endPtr,s) msg

-- The Get Monad ---------------------------------------------------------------

data GetResult a = GetResult {-# UNPACK #-} !S !a
  deriving Functor

newtype Get a = Get
  { runGet :: Ptr Word
           -> H.LinearHashTable Word Noun
           -> S
           -> IO (GetResult a)
  }

instance Functor Get where
    fmap f g = Get $ \end tbl s -> do
        GetResult s' a <- runGet g end tbl s
        return $ GetResult s' (f a)
    {-# INLINE  fmap #-}

instance Applicative Get where
    pure x = Get (\_ _ s -> return $ GetResult s x)
    {-# INLINE pure #-}

    Get f <*> Get g = Get $ \end tbl s1 -> do
        GetResult s2 f' <- f end tbl s1
        GetResult s3 g' <- g end tbl s2
        return $ GetResult s3 (f' g')
    {-# INLINE (<*>) #-}

    Get f *> Get g = Get $ \end tbl s1 -> do
        GetResult s2 _ <- f end tbl s1
        g end tbl s2
    {-# INLINE (*>) #-}

instance Monad Get where
    return = pure
    {-# INLINE return #-}

    (>>) = (*>)
    {-# INLINE (>>) #-}

    Get x >>= f = Get $ \end tbl s -> do
        GetResult s' x' <- x end tbl s
        runGet (f x') end tbl s'
    {-# INLINE (>>=) #-}

    fail msg = Get $ \end tbl s ->
      badEncoding end s msg
    {-# INLINE fail #-}

--------------------------------------------------------------------------------

type Bits = Vector Bool

getPos :: Get Word
getPos = Get $ \_ _ s ->
  pure (GetResult s (pos s))

insRef :: Word -> Noun -> Get ()
insRef pos now = Get \_ tbl s -> do
  H.insert tbl pos now
  pure $ GetResult s ()

getRef :: Word -> Get Noun
getRef ref = Get \_ tbl s -> do
  H.lookup tbl ref >>= \case
    Nothing -> fail "Invalid Reference"
    Just no -> pure (GetResult s no)

advance :: Word -> Get ()
advance n = Get \_ _ s -> do
  let newUsed = n + usedBits s
      newS    = s { pos      = pos s + n
                  , usedBits = newUsed `mod` 64
                  , currPtr  = plusPtr (currPtr s)
                                 (fromIntegral $ newUsed `div` 64)
                  }

  pure (GetResult newS ())

--------------------------------------------------------------------------------

-- TODO Should this be (>= end) or (> end)?
peekCurWord :: Get Word
peekCurWord = Get \end _ s ->
  if ptrToWordPtr (currPtr s) >= ptrToWordPtr end
  then pure (GetResult s 0)
  else GetResult s <$> peek (currPtr s)

-- TODO Same question as above.
peekNextWord :: Get Word
peekNextWord = Get \end _ s ->
  if ptrToWordPtr (currPtr s) > ptrToWordPtr end
  then pure (GetResult s 0)
  else GetResult s <$> peek (currPtr s `plusPtr` 1)

peekUsedBits :: Get Word
peekUsedBits = Get \_ _ s -> pure (GetResult s (usedBits s))

{-|
    Get a bit.

    - Peek the current word.
    - Right-shift by the bit-offset.
    - Mask the high bits.
-}
dBit :: Get Bool
dBit = do
  wor <- peekCurWord
  use <- fromIntegral <$> peekUsedBits
  advance 1
  pure (0 /= shiftR wor use .&. 1)

{-|
    Get n bits, where n > 64:

    - Get (n/64) words.
    - Advance by n bits.
    - Calculate an offset (equal to the current bit-offset)
    - Calculate the length (equal to n)
    - Construct a bit-vector using the buffer*length*offset.
-}
dBits :: Word -> Get Bits
dBits = undefined

{-|
    In order to peek at the next Word64:

    - If we are past the end of the buffer:
      - Return zero.
    - If the bit-offset is zero:
      - Just peek.
    - If we are pointing to the last word:
      - Peek and right-shift by the bit offset.
    - Otherwise,
      - Peek the current word *and* the next word.
      - Right-shift the current word by the bit-offset.
      - Left-shift the next word by the bit-offset.
      - Binary or the resulting two words.
-}
peekWord :: Get Word
peekWord = do
  off <- peekUsedBits
  cur <- peekCurWord
  if off == 0 then pure cur else do
  nex <- peekNextWord
  advance 64
  pure (dropLowBits off cur .|. dropHighBits off nex)

dropLowBits :: Word -> Word -> Word
dropLowBits bits wor = shiftR wor (fromIntegral bits :: Int)

takeLowBits :: Word -> Word -> Word
takeLowBits 64  wor = wor
takeLowBits wid wor = (2^wid - 1) .&. wor

takeHighBits :: Word -> Word -> Word
takeHighBits off wor = dropLowBits (64-off) wor

dropHighBits :: Word -> Word -> Word
dropHighBits off wor = takeLowBits (64-off) wor

{-|
  Make a word from the next n bits (where n <= 64).

  - Peek at the next word.
  - Mask the n lowest bits from the word.
  - Advance by that number of bits.
  - Return the word.
-}
dWordBits :: Word -> Get Word
dWordBits n = do
  w <- peekWord
  advance n
  pure (takeLowBits n w)

--------------------------------------------------------------------------------

bitsToAtom :: Bits -> Atom
bitsToAtom = undefined

--------------------------------------------------------------------------------

{-
    Get the exponent-prefix of an atom:

    - Peek at the next word.
    - Calculate the number of least-significant bits in that word (there's
      a primitive for this).
    - Advance by that number of bits.
    - Return the number of bits
-}
dExp :: Get Word
dExp = do
  W# w <- peekWord
  let res = W# (ctz# w)
  advance res
  pure res

dAtomLen :: Get Word
dAtomLen = do
  e <- dExp
  p <- dWordBits (e-1)
  pure (2^e .|. p)

dRef :: Get Word
dRef = dAtomLen >>= dWordBits

dAtom :: Get Atom
dAtom = do
  n <- dAtomLen
  b <- dBits n
  pure (bitsToAtom b)

dCell :: Get Noun
dCell = Cell <$> dNoun <*> dNoun

{-|
    Get a Noun.

    - Get a bit
    - If it's zero, get an atom.
    - Otherwise, get another bit.
    - If it's zero, get a cell.
    - If it's one, get an atom.
-}
dNoun :: Get Noun
dNoun = do
  p <- getPos

  let yield r = insRef p r >> pure r

  dBit >>= \case
    False -> (Atom <$> dAtom) >>= yield
    True  -> dBit >>= \case
      False -> dCell >>= yield
      True  -> dRef >>= getRef

{-
    Count leading zero bits.

    Read a 64 bit word from the buffer and get the number of leading
    zeros in that word. This works as long as no atom is larger than
    2 zettabytes.

    - TODO Need to handle the edge-case where there are less than 64 bits
      remaining in the buffer. Those extra bytes need to be zeros. One way
      to handle this might be to add a zero word to the end of the buffer,
      but that would require a re-alloc. Probably the right way is to
      write new `peek` primitives that handle this case.

    - TODO Error out if we hit the end *and* the word is all zeros.

    Alright, let's pseudo-code this out:

      Grab the next 64 bits. Pill files are always LSB-first
-}
