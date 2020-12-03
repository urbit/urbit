{-# OPTIONS_GHC -O2 #-}

{-|
    Fast implementation of `cue :: Atom -> Maybe Noun`.

    Implementation is based on the approach used in `flat`.
-}
module Urbit.Noun.Cue (cue, cueExn, cueBS, cueBSExn, DecodeErr) where

import ClassyPrelude

import Urbit.Atom
import Urbit.Noun.Core

import Control.Monad.Fail (MonadFail (fail))
import Data.Bits          (shiftL, shiftR, (.&.), (.|.))
import Data.Function      ((&))
import Foreign.Ptr        (Ptr, castPtr, plusPtr, ptrToWordPtr)
import Foreign.Storable   (peek)
import GHC.Prim           (ctz#)
import GHC.Word           (Word(..))
import System.IO.Unsafe   (unsafePerformIO)
import Text.Printf        (printf)

import qualified Data.ByteString.Unsafe as BS
import qualified Data.HashTable.IO      as H
import qualified Data.Vector.Primitive  as VP


--------------------------------------------------------------------------------

cueBS :: ByteString -> Either DecodeErr Noun
cueBS = doGet dNoun

cueBSExn :: MonadIO m => ByteString -> m Noun
cueBSExn bs =
  cueBS bs & \case
    Left e  -> throwIO e
    Right x -> pure x

cue :: Atom -> Either DecodeErr Noun
cue = cueBS . atomBytes

cueExn :: MonadIO m => Atom -> m Noun
cueExn = cueBSExn . atomBytes


-- Debugging -------------------------------------------------------------------

{-# INLINE debugM #-}
debugM :: Monad m => String -> m ()
debugM _ = pure ()

{-# INLINE debugMId #-}
debugMId :: (Monad m, Show a) => String -> m a -> m a
debugMId _ a = a


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

data DecodeErr
    = InfiniteCue Env
    | BadEncoding Env String
  deriving (Show, Eq, Ord)

data GetResult a = GetResult {-# UNPACK #-} !S !a
  deriving (Show, Functor)

newtype Get a = Get
  { runGet :: Ptr Word
           -> H.BasicHashTable Word Noun
           -> S
           -> IO (GetResult a)
  }

doGet :: Get a -> ByteString -> Either DecodeErr a
doGet m bs =
  unsafePerformIO $ try $ BS.unsafeUseAsCStringLen bs $ \(ptr, len) -> do
    let endPtr = ptr `plusPtr` len
    let sz = max 50
           $ min 10_000_000
           $ length bs `div` 6
    tbl <- H.newSized sz
    GetResult _ r <- runGet m endPtr tbl (S (castPtr ptr) 0 0)
    pure r

--------------------------------------------------------------------------------

instance Exception DecodeErr

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

instance MonadFail Get where
    fail msg = Get $ \end tbl s -> do
      badEncoding end s msg
    {-# INLINE fail #-}

instance MonadIO Get where
  liftIO io = Get $ \end tbl s -> GetResult s <$> io
  {-# INLINE liftIO #-}

--------------------------------------------------------------------------------

{-# INLINE badEncoding #-}
badEncoding :: Ptr Word -> S -> String -> IO a
badEncoding !endPtr s msg = throwIO $ BadEncoding (endPtr,s) msg

--------------------------------------------------------------------------------

{-# INLINE getPos #-}
getPos :: Get Word
getPos = Get $ \_ _ s ->
  pure (GetResult s (pos s))

{-# INLINE insRef #-}
insRef :: Word -> Noun -> Get ()
insRef !pos !now = Get $ \_ tbl s -> do
  H.insert tbl pos now
  pure $ GetResult s ()

{-# INLINE getRef #-}
getRef :: Word -> Get Noun
getRef !ref = Get $ \x tbl s -> do
  H.lookup tbl ref >>= \case
    Nothing -> runGet (fail ("Invalid Reference: " <> show ref)) x tbl s
    Just no -> pure (GetResult s no)

{-# INLINE advance #-}
advance :: Word -> Get ()
advance 0 = debugM "advance: 0" >> pure ()
advance !n = Get $ \_ _ s -> do
  debugM ("advance: " <> show n)
  let newUsed = n + usedBits s
      newS    = s { pos      = pos s + n
                  , usedBits = newUsed `mod` 64
                  , currPtr  = plusPtr (currPtr s)
                                 (8 * (fromIntegral (newUsed `div` 64)))
                  }

  pure (GetResult newS ())

--------------------------------------------------------------------------------

{-# INLINE guardInfinite #-}
guardInfinite :: Ptr Word -> Ptr Word -> S -> IO ()
guardInfinite end cur s =
    when (cur >= (end `plusPtr` 16)) $ do
        throwIO (InfiniteCue (end, s))

-- TODO Should this be (>= end) or (> end)?
{-# INLINE peekCurWord #-}
peekCurWord :: Get Word
peekCurWord = Get $ \end _ s -> do
 debugMId "peekCurWord" $ do
  guardInfinite end (currPtr s) s
  if ptrToWordPtr (currPtr s) >= ptrToWordPtr end
  then pure (GetResult s 0)
  else GetResult s <$> peek (currPtr s)

-- TODO Same question as above.
{-# INLINE peekNextWord #-}
peekNextWord :: Get Word
peekNextWord = Get $ \end _ s -> do
 debugMId "peekNextWord" $ do
  let pTarget = currPtr s `plusPtr` 8
  guardInfinite end pTarget s
  if ptrToWordPtr pTarget >= ptrToWordPtr end
  then pure (GetResult s 0)
  else GetResult s <$> peek pTarget

{-# INLINE peekUsedBits #-}
peekUsedBits :: Get Word
peekUsedBits =
 debugMId "peekUsedBits" $ do
  Get $ \_ _ s -> pure (GetResult s (usedBits s))

{-|
    Get a bit.

    - Peek the current word.
    - Right-shift by the bit-offset.
    - Mask the high bits.
-}
{-# INLINE dBit #-}
dBit :: Get Bool
dBit = do
  debugMId "dBit" $ do
    wor <- peekCurWord
    use <- fromIntegral <$> peekUsedBits
    advance 1
    pure (0 /= shiftR wor use .&. 1)

{-# INLINE dWord #-}
dWord :: Get Word
dWord = do
  debugMId "dWord" $ do
    res <- peekWord
    advance 64
    pure res

{-|
    Get n bits, where n > 64:

    - Get (n/64) words.
    - Advance by n bits.
    - Calculate an offset (equal to the current bit-offset)
    - Calculate the length (equal to n)
    - Construct a bit-vector using the buffer*length*offset.
-}
{-# INLINE dAtomBits #-}
dAtomBits :: Word -> Get Atom
dAtomBits !(fromIntegral -> bits) = do
    debugMId ("dAtomBits(" <> show bits <> ")") $ do
      fmap wordsAtom $
        VP.generateM bufSize $ \i -> do
          debugM (show i)
          if (i == lastIdx && numExtraBits /= 0)
          then dWordBits (fromIntegral numExtraBits)
          else dWord
  where
    bufSize      = numFullWords + min 1 numExtraBits
    lastIdx      = bufSize - 1
    numFullWords = bits `div` 64
    numExtraBits = bits `mod` 64

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
{-# INLINE peekWord #-}
peekWord :: Get Word
peekWord = do
 debugMId "peekWord" $ do
  off <- peekUsedBits
  cur <- peekCurWord
  nex <- peekNextWord
  let res = swiz off (cur, nex)
  debugM ("\t" <> (take 10 $ reverse $ printf "%b" (fromIntegral res :: Integer)) <> "..")
  pure res

{-# INLINE swiz #-}
swiz :: Word -> (Word, Word) -> Word
swiz !(fromIntegral -> off) (!low, !hig) =
  (.|.) (shiftR low off) (shiftL hig (64-off))

{-# INLINE takeLowBits #-}
takeLowBits :: Word -> Word -> Word
takeLowBits 64   !wor = wor
takeLowBits !wid !wor = (2^wid - 1) .&. wor

{-|
  Make a word from the next n bits (where n <= 64).

  - Peek at the next word.
  - Mask the n lowest bits from the word.
  - Advance by that number of bits.
  - Return the word.
-}
{-# INLINE dWordBits #-}
dWordBits :: Word -> Get Word
dWordBits !n = do
 debugMId ("dWordBits(" <> show n <> ")") $ do
  w <- peekWord
  advance n
  debugM ("dWordBits: " <> show (takeLowBits n w))
  pure (takeLowBits n w)


-- Fast Cue --------------------------------------------------------------------

{-|
    Get the exponent-prefix of an atom:

    - Peek at the next word.
    - Calculate the number of least-significant bits in that word (there's
      a primitive for this).
    - Advance by that number of bits.
    - Return the number of bits
-}
{-# INLINE dExp #-}
dExp :: Get Word
dExp = do
 debugMId "dExp" $ do
  W# w <- peekWord
  let res = W# (ctz# w)
  advance (res+1)
  pure res

{-# INLINE dAtomLen #-}
dAtomLen :: Get Word
dAtomLen = do
 debugMId "dAtomLen" $ do
  dExp >>= \case
    0 -> pure 0
    e -> do p <- dWordBits (e-1)
            pure (2^(e-1) .|. p)

{-# INLINE dRef #-}
dRef :: Get Word
dRef = debugMId "dRef" (dAtomLen >>= dWordBits)

{-# INLINE dAtom #-}
dAtom :: Get Atom
dAtom = do
 debugMId "dAtom" $ do
  dAtomLen >>= \case
    0 -> pure 0
    n -> dAtomBits n

{-# INLINE dCell #-}
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
    False -> do debugM "It's an atom"
                (Atom <$> dAtom) >>= yield
    True  -> dBit >>= \case
      False -> do debugM "It's a cell"
                  dCell >>= yield
      True  -> do debugM "It's a backref"
                  dRef >>= getRef
