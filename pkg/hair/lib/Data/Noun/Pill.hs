{-# LANGUAGE MagicHash #-}

{-
    TODO Handle 32-bit architectures
    TODO A faster version of this is possible:

      - Get the byte-length of a file.
      - Round up to a multiple of 8 (or 4 if 32bit cpu)
      - Allocate a mutable vector of Word8 with that size.
      - Read the file into the array.
      - Manually cast to an array of Word.
      - On big endian, update each words with `System.Endian.fromLE64`.
      - If there are trailing 0 words, adjust the vector size to delete them.
      - unsafeFreeze the vector.
      - Run `byteArrayToBigNat#` on the underlying byte array.
      - Convert the BigNat to a Natural, to an Atom.
      - The whole thing becomes zero-copy for little endian machines, with
        one zero-copy transformation of the whole structure on big-endian
        machines.
-}

module Data.Noun.Pill where

import ClassyPrelude
import Data.Noun hiding (toList, fromList)
import Data.Noun.Atom
import Data.Noun.Jam hiding (main)
import Data.Flat
import Control.Monad.Except
import Control.Lens hiding (index, Index)
import Data.Either.Extra (mapLeft)
import GHC.Natural
import Data.Bits
import GHC.Integer.GMP.Internals
import GHC.Int
import GHC.Word
import GHC.Exts (sizeofByteArray#)

import qualified Data.Vector              as V
import qualified Data.Primitive.ByteArray as Prim
import qualified Data.Vector.Primitive    as VP
import qualified Data.Vector.Unboxed      as VU
import qualified Data.ByteString          as BS

import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck

--------------------------------------------------------------------------------

stripTrailingZeros :: IsSequence seq
                   => Int ~ Index seq
                   => (Eq (Element seq), Num (Element seq))
                   => seq -> seq
stripTrailingZeros buf = take (len - go 0 (len - 1)) buf
  where
    len = length buf
    go n i | i < 0                  = n
           | 0 == unsafeIndex buf i = go (n+1) (i-1)
           | otherwise              = n

--------------------------------------------------------------------------------

wordsToBigNat :: VP.Vector Word -> BigNat
wordsToBigNat v@(VP.Vector off (I# len) (Prim.ByteArray buf)) =
  case VP.length v of
    0 -> zeroBigNat
    1 -> wordToBigNat (case VP.unsafeIndex v 0 of W# w -> w)
    n -> if off /= 0 then error "words2Nat: bad-vec" else
         byteArrayToBigNat# buf len

bigNatToWords :: BigNat -> VP.Vector Word
bigNatToWords (BN# bArr) = VP.Vector 0 (I# (sizeofByteArray# bArr) `div` 8)
                           $ Prim.ByteArray bArr

--------------------------------------------------------------------------------

bigNatToBits :: BigNat -> VU.Vector Bool
bigNatToBits = undefined

bitsToBigNat :: BigNat -> VU.Vector Bool
bitsToBigNat = undefined

--------------------------------------------------------------------------------

naturalToBigNat :: Natural -> BigNat
naturalToBigNat (NatS# w)  = wordToBigNat w
naturalToBigNat (NatJ# bn) = bn

bigNatToNatural :: BigNat -> Natural
bigNatToNatural bn =
    case sizeofBigNat# bn of
      0# -> 0
      1# -> NatS# (bigNatToWord bn)
      _  -> NatJ# bn

--------------------------------------------------------------------------------

wordsToNatural :: VP.Vector Word -> Natural
wordsToNatural = bigNatToNatural . wordsToBigNat

naturalToWords :: Natural -> VP.Vector Word
naturalToWords = bigNatToWords . naturalToBigNat

--------------------------------------------------------------------------------

dumbPackWord :: ByteString -> Word
dumbPackWord bs = go 0 0 (toList bs)
  where
    go acc i []     = acc
    go acc i (x:xs) = go (acc .|. shiftL (fromIntegral x) (8*i)) (i+1) xs

-- TODO This assumes 64-bit words
packWord :: ByteString -> Word
packWord buf = go 0 0
  where
    top        = min 8 (length buf)
    i idx off  = shiftL (fromIntegral $ BS.index buf idx) off
    go acc idx = if idx >= top then acc else
                 go (acc .|. i idx (8*idx)) (idx+1)


-- TODO This assumes 64-bit words
unpackWord :: Word -> ByteString
unpackWord wor = reverse $ fromList $ go 0 []
  where
    go i acc | i >= 8    = acc
    go i acc | otherwise = go (i+1) (fromIntegral (shiftR wor (i*8)) : acc)

--------------------------------------------------------------------------------

bytesToWords :: ByteString -> VP.Vector Word
bytesToWords bytes =
  VP.generate (1 + length bytes `div` 8) $ \i ->
    packWord (BS.drop (i*8) bytes)

fromPrimVec :: Prim a => VP.Vector a -> V.Vector a
fromPrimVec vp = V.generate (VP.length vp) (VP.unsafeIndex vp)

wordsToBytes :: VP.Vector Word -> ByteString
wordsToBytes = stripTrailingZeros . concat . fmap unpackWord . fromPrimVec

--------------------------------------------------------------------------------

dumbPackAtom :: ByteString -> Atom
dumbPackAtom bs = go 0 0 (toList bs)
  where
    go acc i []     = acc
    go acc i (x:xs) = go (acc .|. shiftL (fromIntegral x) (8*i)) (i+1) xs

packAtom :: ByteString -> Atom
packAtom = MkAtom . wordsToNatural . bytesToWords . stripTrailingZeros

unpackAtom :: Atom -> ByteString
unpackAtom (MkAtom a) = trace "unpack" $! wordsToBytes (naturalToWords a)

--------------------------------------------------------------------------------

bsToNoun :: ByteString -> Maybe Noun
bsToNoun = cue . packAtom

nounToBs :: Noun -> ByteString
nounToBs = unpackAtom . jam

loadFile :: FilePath -> IO Atom
loadFile = fmap packAtom . readFile

loadJam :: FilePath -> IO (Maybe Noun)
loadJam = fmap cue . loadFile

dumpJam :: FilePath -> Noun -> IO ()
dumpJam pat = writeFile pat . unpackAtom . jam

dumpFlat :: Flat a => FilePath -> a -> IO ()
dumpFlat pat = writeFile pat . flat

loadFlat :: Flat a => FilePath -> IO (Either Text a)
loadFlat pat = do
  bs <- readFile pat
  pure $ mapLeft tshow $ unflat bs

data Pill = Brass | Ivory | Solid

instance Show Pill where
  show = \case
    Brass -> "./bin/brass.pill"
    Solid -> "./bin/solid.pill"
    Ivory -> "./bin/ivory.pill"

tryLoadPill :: Pill -> IO ()
tryLoadPill pill = do
    a@(MkAtom nat) <- loadFile (show pill)
    putStrLn "loaded"
    print (a > 0)
    putStrLn "evaled"
    print (take 10 $ VP.toList $ naturalToWords nat)

tryCuePill :: Pill -> IO ()
tryCuePill pill =
    loadJam (show pill) >>= \case Nothing       -> print "nil"
                                  Just (Atom _) -> print "atom"
                                  _             -> print "cell"

-- Tests -----------------------------------------------------------------------

instance Arbitrary ByteString where
  arbitrary = fromList <$> arbitrary

instance Arbitrary BigNat where
  arbitrary = naturalToBigNat <$> arbitrary

instance Show BigNat where
  show = show . NatJ#

roundTrip :: Eq a => (a -> b) -> (b -> a) -> (a -> Bool)
roundTrip dump load x = x == load (dump x)

equiv :: Eq b => (a -> b) -> (a -> b) -> (a -> Bool)
equiv f g x = f x == g x

check :: Atom -> Atom
check = toAtom . (id :: Integer -> Integer) . fromAtom

clean :: IsSequence seq
      => Int ~ Index seq
      => (Eq (Element seq), Num (Element seq))
      => seq -> seq
clean = stripTrailingZeros

prop_packWordSane = equiv packWord dumbPackWord . fromList
prop_packWord     = roundTrip unpackWord packWord
prop_unpackWord   = roundTrip packWord (clean . unpackWord) . clean . take 8

prop_unpackBigNat = roundTrip bigNatToWords wordsToBigNat

prop_packBigNat   = roundTrip (wordsToBigNat . VP.fromList)
                              (clean . VP.toList . bigNatToWords)
                  . clean

prop_implodeBytes = roundTrip bytesToWords wordsToBytes . clean

prop_explodeBytes = roundTrip (wordsToBytes . VP.fromList)
                              (clean . VP.toList . bytesToWords)
                  . clean

prop_packAtomSane = equiv packAtom dumbPackAtom . fromList
prop_unpackAtom   = roundTrip unpackAtom packAtom
prop_packAtom     = roundTrip packAtom unpackAtom . clean

--------------------------------------------------------------------------------

main :: IO ()
main = $(defaultMainGenerator)
