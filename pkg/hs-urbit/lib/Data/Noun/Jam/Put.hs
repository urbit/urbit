{-# LANGUAGE MagicHash #-}

module Data.Noun.Jam.Put where

import ClassyPrelude
import GHC.Prim
import GHC.Natural
import GHC.Integer.GMP.Internals

import Control.Lens          (view, to, from, (&))
import Control.Monad         (guard)
import Data.Bits             (shiftL, shiftR, setBit, clearBit, xor, (.|.), (.&.))
import Data.Map              (Map)
import Data.Noun.Atom        ( Atom(MkAtom), wordBitWidth, wordBitWidth#
                             , atomBitWidth#, takeBitsWord )
import Data.Noun.Atom        (toAtom, takeBits, bitWidth)
import Data.Noun             (Noun(Atom, Cell))
import Data.Noun.Pill        (bigNatWords, atomBS)
import Data.Vector.Primitive ((!))
import Foreign.Marshal.Alloc (callocBytes, free)
import Foreign.Ptr           (Ptr, castPtr, plusPtr, ptrToWordPtr)
import Foreign.Storable      (peek, poke)
import GHC.Int               (Int(I#))
import GHC.Word              (Word(W#))
import System.IO.Unsafe      (unsafePerformIO)

import qualified Data.Hashable          as Hash
import qualified Data.Map               as M
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
  { runPut :: H.LinearHashTable Noun Word
           -> S
           -> IO (PutResult a)
  }

--------------------------------------------------------------------------------

{-# INLINE getRef #-}
getRef :: Noun -> Put (Maybe Word)
getRef n = Put \tbl s -> do
    pos <- pure (pos s)
    traceM ("getRef: " <> show n <> " @" <> show pos)
    res <- H.lookup tbl n
    pure $ PutResult s $ case res of
                           Just w | w<pos -> Just w
                           _              -> Nothing

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
  -- traceM ("writeBit: " <> show b)
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
    -- traceM ("writeWord: " <> show wor)
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

    -- traceM ("writeBitsFromWord: " <> show wid <> ", " <> show wor)

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
    -- traceM "writeAtomWord"
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
  -- traceM "writeAtomBigNat"
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

doPut :: Map Noun Word -> Word -> Put () -> ByteString
doPut tbl sz m =
    unsafePerformIO $ do
        tbl <- H.fromListWithSizeHint (M.size tbl) (mapToList tbl)
        buf <- callocBytes (fromIntegral $ 4 * wordSz*8)
        _   <- runPut (m >> mbFlush) tbl (S buf 0 0 0)
        BS.unsafePackCStringFinalizer (castPtr buf) (2*byteSz) (free buf)
  where
    wordSz = fromIntegral (sz `divUp` 64)
    byteSz = fromIntegral (sz `divUp` 8)
    divUp x y = (x `div` y) + (if x `mod` y == 0 then 0 else 1)

    mbFlush :: Put ()
    mbFlush = do
      shouldFlush <- (/= 0) . off <$> getS
      when shouldFlush flush


--------------------------------------------------------------------------------

{-
    TODO Handle back references
-}
writeNoun :: Noun -> Put ()
writeNoun n = do
    -- traceM "writeNoun"

    mRef <- getRef n

    case (mRef, n) of
        (Nothing, Atom a)                                 -> writeAtom a
        (Nothing, Cell h t)                               -> writeCell h t
        (Just bk, Atom a) | bitWidth a <= wordBitWidth bk -> writeAtom a
        (Just bk, _)                                      -> writeBackRef bk

{-# INLINE writeMat #-}
writeMat :: Atom -> Put ()
writeMat 0 = do
    -- traceM "writeMat: 0"
    writeBit True
writeMat atm = do
    -- traceM ("writeMat: " <> show atm)
    writeBitsFromWord (preWid+1) (shiftL 1 preWid)
    writeBitsFromWord (preWid-1) atmWid
    writeAtomBits atm
  where
    atmWid = bitWidth atm
    preWid = fromIntegral (wordBitWidth atmWid)

{-# INLINE writeCell #-}
writeCell :: Noun -> Noun -> Put ()
writeCell h t = do
    -- traceM "writeCell"
    writeBit True
    writeBit False
    writeNoun h
    writeNoun t

{-# INLINE writeAtom #-}
writeAtom :: Atom -> Put ()
writeAtom a = do
    -- traceM "writeAtom"
    writeBit False
    writeMat a

{-# INLINE writeBackRef #-}
writeBackRef :: Word -> Put ()
writeBackRef a = do
    p <- pos <$> getS
    traceM ("writeBackRef: " <> show a <> " @" <> show p)
    writeBit True
    writeBit True
    writeMat (toAtom a)

--------------------------------------------------------------------------------

jamBS :: Noun -> ByteString
jamBS n = trace (show $ sort $ swap <$> mapToList tbl)
        $ doPut tbl sz (writeNoun n)
  where (sz, tbl) = preJam n

jam :: Noun -> Atom
jam = view (to jamBS . from atomBS)

--------------------------------------------------------------------------------

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

{-# INLINE refSz# #-}
refSz# :: Word# -> Word#
refSz# w = 2## `plusWord#` (matSz# (MkAtom (NatS# w)))

preJam :: Noun -> (Word, Map Noun Word)
preJam = go 0 mempty
  where
    insertNoun :: Noun -> Word -> Map Noun Word -> Map Noun Word
    insertNoun n i tbl = lookup n tbl
                       & maybe (insertMap n i tbl) (const tbl)

    go :: Word -> Map Noun Word -> Noun -> (Word, Map Noun Word)
    go off oldTbl noun =
      case lookup noun oldTbl of
        Nothing ->
          let tbl = insertNoun noun off oldTbl in
          case noun of
            Atom atm ->
              (1 + matSz atm, tbl)
            Cell l r ->
              let (lSz, tbl')  = go (2+off)     tbl  l in
              let (rSz, tbl'') = go (2+off+lSz) tbl' r in
              (2 + lSz + rSz, tbl'')
        Just (W# ref) ->
          let refSz = W# (wordBitWidth# ref) in
          case noun of
            Atom atm ->
              let worSz = matSz atm in
              if worSz > refSz
              then (2 + refSz, oldTbl)
              else (1 + worSz, oldTbl)
            Cell _ _ ->
              (2 + refSz, oldTbl)


-- Fast Pre-Jam ----------------------------------------------------------------

{-
    An `SHN` is a noun and some pre-computed information.

    - `size` is the serialized size without backreferences, we use this
      for fast equality checks.
    - `jmSz` is the serialized size, we use this to allocate a buffer
      at the end.
    - `hash` is a precomputed noun hash. We use this to get better,
      cheaper hashes for our hashtable.
    - `noun` is the actual noun.
-}
data SHN = SHN
    { size :: {-# UNPACK #-} !Word
    , jmSz :: {-# UNPACK #-} !Word
    , hash :: {-# UNPACK #-} !Int
    , noun :: {-# UNPACK #-} !Noun
    }
  deriving (Show)

instance Hashable SHN where
  hash (SHN _ _ h _) = h
  {-# INLINE hash #-}
  hashWithSalt     = defaultHashWithSalt
  {-# INLINE hashWithSalt #-}

instance Eq SHN where
  x == y = (size x == size y) && (noun x == noun y)

preJam' :: Noun -> IO (SHN, H.LinearHashTable Word Word)
preJam' top = do
    nodes :: H.LinearHashTable SHN  Word <- H.new
    backs :: H.LinearHashTable Word Word <- H.new

    let goAtom :: Word -> Atom -> IO SHN
        goAtom pos a@(MkAtom nat) = do
            let atmSz = matSz a
            let res   = SHN (1+atmSz) (1+atmSz) (Hash.hash nat) (Atom a)
            H.lookup nodes res >>= \case
                Nothing -> do
                    H.insert nodes res pos
                    pure (traceShowId res)
                Just bak -> do
                    let refSz = matSz (toAtom bak)
                    if refSz < atmSz
                    then do H.insert backs pos bak
                            pure (traceShowId (res{jmSz=2+refSz}))
                    else pure (traceShowId res)

        goCell :: Word -> Noun -> Noun -> IO SHN
        goCell pos h t = do
            SHN hSz hJmSz hHash _ <- go (pos+2) h
            SHN tSz tJmSz tHash _ <- go (pos+2+hSz) t
            let sz   = 2+hSz+tSz
            let jmSz = 2+hJmSz+tJmSz
            let res  = SHN sz jmSz (combine hHash tHash) (Cell h t)
            H.lookup nodes res >>= \case
                Nothing -> do
                    H.insert nodes res pos
                    pure (traceShowId res)
                Just bak -> do
                    let refSz = matSz (toAtom bak)
                    H.insert backs pos bak
                    pure (traceShowId (res{jmSz=2+refSz}))

        go :: Word -> Noun -> IO SHN
        go p (Atom a)   = goAtom p a
        go p (Cell h t) = goCell p h t

    res <- go 0 top
    pure (res, backs)

-- Stolen from Hashable Library ------------------------------------------------

{-# INLINE combine #-}
combine :: Int -> Int -> Int
combine h1 h2 = (h1 * 16777619) `xor` h2

{-# INLINE defaultHashWithSalt #-}
defaultHashWithSalt :: Hashable a => Int -> a -> Int
defaultHashWithSalt salt x = salt `combine` Hash.hash x


{-
    I suspect that hashing these big atoms recursively is going to be the bottleneck:
      Unless you have a good hashing system.
        Which we totally do in the nock runtime.
      Checking the hash for the top-level node precomputes the hashes for
        everything else, recursively.
      This is really smart.
        Maybe I could implement this as well?
          But hashing traverses the whole structure.
        So, now we have
          1. precompute hashes.
          2. precompute size and backref table.
          3. serialize
        This seems excessive.
          We insert into the backref table right away, but actually:
            Backreferences can't exist until the whole node is processed.

          Which implies a smarter algorithm:
            - Setup a atom dup table
                atoms :: Hashtable BigNum Word
            - Setup a cell dup table
                cells :: Hashtable (Noun, Noun) Word
            - Setup a backref table (map from dup. pos to orig. pos)
                backs :: Hashtable Word Word
            - go :: Noun -> ST s (Hash, Word)
              - If atom,
                - Compute size and hash
                - Check atom table for backref
                - If atom in `atoms` table:
                  - If backref smaller than atom
                    - Insert (pos, bak) into `backs` table.
                    - Return (backref size, atom hash)
                  - If backref not smaller than atom
                    - Return (atom size, atom hash)
                - Otherwise:
                  - Insert atom into `atoms` table.
                  - Return (atom size, atom hash)
              - If cell
                - process head
                - process tail
                - produce size+hash from results
                - Check cell table for backref
                - If backref exists
                  - Insert `(pos, bak)` into `backs` table
                  - Return (backref size, cell hash)
                - Else
                  - Return (cell size, cell hash)

          Then, to serialize:
            - Allocate a buffer of `size` bits
            - If current pos in `backs` table:
              - Write `11`
              - Write backref (mat)
            - Otherwise:
              - If Atom:
                - Write `0`
                - Write atom (mat)
              - If Cell
                - Write `10`
                - Write head
                - Write tail
-}
