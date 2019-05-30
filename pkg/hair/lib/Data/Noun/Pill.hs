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
import Data.Flat hiding (from)
import Control.Monad.Except
import Control.Lens hiding (index, Index)
import Data.Either.Extra (mapLeft)
import GHC.Natural
import Data.Bits
import GHC.Integer.GMP.Internals
import GHC.Int
import GHC.Word
import GHC.Exts (sizeofByteArray#)
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.Vector              as V
import qualified Data.Primitive.Types     as Prim
import qualified Data.Primitive.ByteArray as Prim
import qualified Data.Vector.Primitive    as VP
import qualified Data.Vector.Unboxed      as VU
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Unsafe   as BU

import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck

--------------------------------------------------------------------------------

{-
    A `Pill` is a bytestring without trailing zeros.
-}
newtype Pill = Pill { unPill :: ByteString }

strip :: (IsSequence seq, Int ~ Index seq, Eq (Element seq), Num (Element seq))
      => seq -> seq
strip buf = take (len - go 0 (len - 1)) buf
  where
    len = length buf
    go n i | i < 0                  = n
           | 0 == unsafeIndex buf i = go (n+1) (i-1)
           | otherwise              = n

pillBytes :: Iso' Pill ByteString
pillBytes = iso to from
  where
    to :: Pill -> ByteString
    to = strip . unPill

    from :: ByteString -> Pill
    from = Pill . strip

instance Eq Pill where
  (==) x y = (x ^. pillBytes) == (y ^. pillBytes)

instance Show Pill where
  show = show . view pillBytes

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

wordsToBytes :: VP.Vector Word -> VP.Vector Word8
wordsToBytes (VP.Vector off sz buf) = VP.Vector (off*8) (sz*8) buf

byteStrToWords :: ByteString -> VP.Vector Word
byteStrToWords bytes =
  VP.generate (1 + length bytes `div` 8) $ \i ->
    packWord (BS.drop (i*8) bytes)

-- TODO Support Big-Endian
bytesBS :: Iso' (VP.Vector Word8) ByteString
bytesBS = iso to from
  where
    to :: VP.Vector Word8 -> ByteString
    to (VP.Vector off sz buf) =
        BS.copy $ BS.drop off $ unsafePerformIO $ BU.unsafePackAddressLen sz ptr
      where
        Prim.Addr ptr = Prim.byteArrayContents buf

    from :: ByteString -> VP.Vector Word8
    from bs = VP.generate (length bs) (BS.index bs)

pillWords :: Iso' Pill (VP.Vector Word)
pillWords = iso to from
  where
    to   = byteStrToWords . view pillBytes
    from = Pill . view bytesBS . wordsToBytes

--------------------------------------------------------------------------------

{-
    This is a stupid, but obviously correct version of `packAtom`.
-}
dumbPackAtom :: Pill -> Atom
dumbPackAtom = go 0 0 . toList . view pillBytes
  where
    go acc i []     = acc
    go acc i (x:xs) = go (acc .|. shiftL (fromIntegral x) (8*i)) (i+1) xs

packAtom :: Pill -> Atom
packAtom = MkAtom . wordsToNatural . byteStrToWords . view pillBytes

unpackAtom :: Atom -> Pill
unpackAtom = view (from pillWords) . naturalToWords . unAtom

--------------------------------------------------------------------------------

readPill :: FilePath -> IO Pill
readPill = fmap Pill . readFile

writePill :: FilePath -> Pill -> IO ()
writePill fp = writeFile fp . view pillBytes

pillToNoun :: Pill -> Maybe Noun
pillToNoun = cue . packAtom

nounToPill :: Noun -> Pill
nounToPill = unpackAtom . jam

loadFile :: FilePath -> IO Atom
loadFile = fmap packAtom . readPill

loadJam :: FilePath -> IO (Maybe Noun)
loadJam = fmap cue . loadFile

dumpJam :: FilePath -> Noun -> IO ()
dumpJam pat = writePill pat . unpackAtom . jam

dumpFlat :: Flat a => FilePath -> a -> IO ()
dumpFlat pat = writeFile pat . flat

loadFlat :: Flat a => FilePath -> IO (Either Text a)
loadFlat pat = do
  bs <- readFile pat
  pure $ mapLeft tshow $ unflat bs

data PillFile = Brass | Ivory | Solid

instance Show PillFile where
  show = \case
    Brass -> "./bin/brass.pill"
    Solid -> "./bin/solid.pill"
    Ivory -> "./bin/ivory.pill"

tryLoadPill :: PillFile -> IO Atom
tryLoadPill pill = do
    a@(MkAtom nat) <- loadFile (show pill)
    putStrLn "loaded"
    print (a > 0)
    putStrLn "evaled"
    print (take 10 $ VP.toList $ naturalToWords nat)
    pure a

tryPackPill :: PillFile -> IO ()
tryPackPill pf = do
  atm <- tryLoadPill pf
  print $ length $ unPill $ unpackAtom atm

tryCuePill :: PillFile -> IO ()
tryCuePill pill =
    loadJam (show pill) >>= \case Nothing       -> print "nil"
                                  Just (Atom _) -> print "atom"
                                  _             -> print "cell"

-- Tests -----------------------------------------------------------------------

instance Arbitrary ByteString where
  arbitrary = fromList <$> arbitrary

instance Arbitrary Pill where
  arbitrary = Pill <$> arbitrary

instance Arbitrary BigNat where
  arbitrary = naturalToBigNat <$> arbitrary

instance Show BigNat where
  show = show . NatJ#

testIso :: Eq a => Iso' a b -> a -> Bool
testIso iso x = x == (x ^. iso . from iso)

roundTrip :: Eq a => (a -> b) -> (b -> a) -> (a -> Bool)
roundTrip dump load x = x == load (dump x)

equiv :: Eq b => (a -> b) -> (a -> b) -> (a -> Bool)
equiv f g x = f x == g x

check :: Atom -> Atom
check = toAtom . (id :: Integer -> Integer) . fromAtom

prop_packWordSane = equiv packWord dumbPackWord . fromList
prop_packWord     = roundTrip unpackWord packWord
prop_unpackWord   = roundTrip packWord (strip . unpackWord) . strip . take 8

prop_unpackBigNat = roundTrip bigNatToWords wordsToBigNat

prop_packBigNat   = roundTrip (wordsToBigNat . VP.fromList)
                              (strip . VP.toList . bigNatToWords)
                  . strip

prop_implodeBytes = roundTrip (view pillWords) (view (from pillWords))

prop_explodeBytes = roundTrip (view (from pillWords) . VP.fromList)
                              (strip . VP.toList . view pillWords)
                  . strip

prop_packAtomSane = equiv packAtom dumbPackAtom . Pill . fromList
prop_unpackAtom   = roundTrip unpackAtom packAtom
prop_packAtom     = roundTrip packAtom unpackAtom . Pill . strip

--------------------------------------------------------------------------------

main :: IO ()
main = $(defaultMainGenerator)
