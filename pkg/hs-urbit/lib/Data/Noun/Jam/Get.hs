module Data.Noun.Jam.Get where

import ClassyPrelude

import Data.Noun        (Noun)
import Data.Bits        (shiftR, (.|.), (.&.))
import Foreign.Ptr      (Ptr, plusPtr, ptrToWordPtr)
import Foreign.Storable (peek)
import Data.Map         (Map)
import Control.Monad    (guard)

import qualified Data.HashTable.IO as H


-- Types -----------------------------------------------------------------------

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

type Env = (Ptr Word, S)

data DecodeExn
    = NotEnoughSpace Env
    | TooMuchSpace Env
    | BadEncoding Env String
  deriving (Show, Eq, Ord)

data GetResult a = GetResult {-# UNPACK #-} !S !a
  deriving Functor

newtype Get a = Get
  { runGet :: Ptr Word
           -> H.LinearHashTable Word Noun
           -> S
           -> IO (GetResult a)
  }

type Bits = Vector Bool

--------------------------------------------------------------------------------

instance Exception DecodeExn

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

badEncoding :: Ptr Word -> S -> String -> IO a
badEncoding endPtr s msg = throwIO $ BadEncoding (endPtr,s) msg

--------------------------------------------------------------------------------

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
  if off == 0 then pure cur else
    do
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
