{-# LANGUAGE MagicHash #-}

{-
    TODO Handle 32-bit architectures
    TODO Handle big-endian.
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
import Data.Flat hiding (from, to)
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

instance Eq Pill where
  (==) x y = (x ^. pillBS) == (y ^. pillBS)

instance Show Pill where
  show = show . view pillBS

--------------------------------------------------------------------------------

strip :: (IsSequence seq, Int ~ Index seq, Eq (Element seq), Num (Element seq))
      => seq -> seq
strip buf = take (len - go 0 (len - 1)) buf
  where
    len = length buf
    go n i | i < 0                  = n
           | 0 == unsafeIndex buf i = go (n+1) (i-1)
           | otherwise              = n

pillBS :: Iso' Pill ByteString
pillBS = iso to from
  where
    to :: Pill -> ByteString
    to = strip . unPill

    from :: ByteString -> Pill
    from = Pill . strip

--------------------------------------------------------------------------------

bigNatWords :: Iso' BigNat (VP.Vector Word)
bigNatWords = iso to from
  where
    to (BN# bArr) = VP.Vector 0 (I# (sizeofByteArray# bArr) `div` 8)
                                (Prim.ByteArray bArr)

    from v@(VP.Vector off (I# len) (Prim.ByteArray buf)) =
      case VP.length v of
        0 -> zeroBigNat
        1 -> wordToBigNat (case VP.unsafeIndex v 0 of W# w -> w)
        n -> if off /= 0 then error "words2Nat: bad-vec" else
             byteArrayToBigNat# buf len

--------------------------------------------------------------------------------

bigNatBits :: Iso' BigNat (VU.Vector Bool)
bigNatBits = undefined

natWords :: Iso' Natural (VP.Vector Word)
natWords = naturalBigNat . bigNatWords

naturalBigNat :: Iso' Natural BigNat
naturalBigNat = iso to from
  where
    to = \case NatS# w  -> wordToBigNat w
               NatJ# bn -> bn

    from bn = case sizeofBigNat# bn of 0# -> 0
                                       1# -> NatS# (bigNatToWord bn)
                                       _  -> NatJ# bn

--------------------------------------------------------------------------------

dumbPackWord :: ByteString -> Word
dumbPackWord bs = go 0 0 (toList bs)
  where
    go acc i []     = acc
    go acc i (x:xs) = go (acc .|. shiftL (fromIntegral x) (8*i)) (i+1) xs

-- TODO This assumes 64-bit words
packedWord :: Iso' ByteString Word
packedWord = iso to from
 where
    from wor = reverse $ fromList $ go 0 []
      where
        go i acc | i >= 8    = acc
        go i acc | otherwise = go (i+1) (fromIntegral (shiftR wor (i*8)) : acc)

    to buf = go 0 0
      where
        top        = min 8 (length buf)
        i idx off  = shiftL (fromIntegral $ BS.index buf idx) off
        go acc idx = if idx >= top then acc else
                     go (acc .|. i idx (8*idx)) (idx+1)

--------------------------------------------------------------------------------

wordsToBytes :: VP.Vector Word -> VP.Vector Word8
wordsToBytes (VP.Vector off sz buf) =
  VP.Vector (off*8) (sz*8) buf

bsToWords :: ByteString -> VP.Vector Word
bsToWords bs =
  VP.generate (1 + length bs `div` 8) $ \i ->
    view packedWord (BS.drop (i*8) bs)

{-
    TODO Support Big-Endian
    TODO This still has a (small) risk of segfaulting. The right thing to
         do is to manually copy the data to the C heap, setup the
         finalizers, and then manually construct a bytestring from
         that pointer.  -- finalizers, and make a bytestring from that.
-}
bytesBS :: Iso' (VP.Vector Word8) ByteString
bytesBS = iso to from
  where
    to :: VP.Vector Word8 -> ByteString
    to (VP.Vector off sz buf) =
        unsafePerformIO $ do
          Prim.Addr ptr <- evaluate $ Prim.byteArrayContents buf
          bs <- BU.unsafePackAddressLen sz ptr
          evaluate $ force $ BS.copy $ BS.drop off bs

    from :: ByteString -> VP.Vector Word8
    from bs = VP.generate (length bs) (BS.index bs)

pillWords :: Iso' Pill (VP.Vector Word)
pillWords = iso toVec fromVec
  where
    toVec   = view (pillBS . to bsToWords)
    fromVec = view (to wordsToBytes . bytesBS . from pillBS)

_CueBytes :: Prism' ByteString Noun
_CueBytes = from pillBS . from pill . _Cue

--------------------------------------------------------------------------------

{-
    This is a stupid, but obviously correct version of `view (from pill)`.
-}
dumbPackAtom :: Pill -> Atom
dumbPackAtom = go 0 0 . toList . view pillBS
  where
    go acc i []     = acc
    go acc i (x:xs) = go (acc .|. shiftL (fromIntegral x) (8*i)) (i+1) xs

atomNat :: Iso' Atom Natural
atomNat = iso unAtom MkAtom

pill :: Iso' Atom Pill
pill = iso toAtom fromPill
  where
    toAtom   = view (atomNat . natWords . from pillWords)
    fromPill = view (pillBS . to bsToWords . from natWords . from atomNat)

--------------------------------------------------------------------------------

_Cue :: Prism' Atom Noun
_Cue = prism' jam cue

_Tall :: Flat a => Prism' ByteString a
_Tall = prism' flat (eitherToMaybe . unflat)
  where
    eitherToMaybe :: Either a b -> Maybe b
    eitherToMaybe (Left x)  = Nothing
    eitherToMaybe (Right x) = Just x

--------------------------------------------------------------------------------

loadPill :: FilePath -> IO Pill
loadPill = fmap Pill . readFile

loadAtom :: FilePath -> IO Atom
loadAtom = fmap (view $ from pillBS . from pill) . readFile

loadNoun :: FilePath -> IO (Maybe Noun)
loadNoun = fmap (preview $ from pillBS . from pill . _Cue) . readFile

loadFlat :: Flat a => FilePath -> IO (Either Text a)
loadFlat = fmap (mapLeft tshow . unflat) . readFile

--------------------------------------------------------------------------------

dumpPill :: FilePath -> Pill -> IO ()
dumpPill fp = writeFile fp . view pillBS

dumpAtom :: FilePath -> Atom -> IO ()
dumpAtom fp = writeFile fp . view (pill . pillBS)

dumpJam :: FilePath -> Noun -> IO ()
dumpJam fp = writeFile fp . view (re _Cue . pill . pillBS)

dumpFlat :: Flat a => FilePath -> a -> IO ()
dumpFlat fp = writeFile fp . flat

--------------------------------------------------------------------------------

data PillFile = Brass | Ivory | Solid

instance Show PillFile where
  show = \case
    Brass -> "./bin/brass.pill"
    Solid -> "./bin/solid.pill"
    Ivory -> "./bin/ivory.pill"

tryLoadPill :: PillFile -> IO Atom
tryLoadPill pill = do
    a@(MkAtom nat) <- loadAtom (show pill)
    putStrLn "loaded"
    print (a > 0)
    putStrLn "evaled"
    print (take 10 $ VP.toList $ nat ^. natWords)
    pure a

tryPackPill :: PillFile -> IO ()
tryPackPill pf = do
  atm <- tryLoadPill pf
  print $ length (atm ^. pill . pillBS)

tryCuePill :: PillFile -> IO ()
tryCuePill pill =
    loadNoun (show pill) >>= \case Nothing       -> print "nil"
                                   Just (Atom _) -> print "atom"
                                   _             -> print "cell"

-- Tests -----------------------------------------------------------------------

instance Arbitrary ByteString where
  arbitrary = fromList <$> arbitrary

instance Arbitrary Pill where
  arbitrary = Pill <$> arbitrary

instance Arbitrary BigNat where
  arbitrary = view naturalBigNat <$> arbitrary

instance Show BigNat where
  show = show . NatJ#

--------------------------------------------------------------------------------

testIso :: Eq a => Iso' a b -> a -> Bool
testIso iso x = x == (x ^. iso . from iso)

roundTrip :: Eq a => (a -> b) -> (b -> a) -> (a -> Bool)
roundTrip dump load x = x == load (dump x)

equiv :: Eq b => (a -> b) -> (a -> b) -> (a -> Bool)
equiv f g x = f x == g x

check :: Atom -> Atom
check = toAtom . (id :: Integer -> Integer) . fromAtom

--------------------------------------------------------------------------------

prop_packWordSane = equiv (view packedWord) dumbPackWord . fromList
prop_packWord     = testIso (from packedWord)
prop_unpackWord   = roundTrip (view packedWord)
                              (strip . view (from packedWord))
                  . strip
                  . take 8

prop_unpackBigNat = testIso bigNatWords

prop_packBigNat   = roundTrip (view (from bigNatWords) . VP.fromList)
                              (strip . VP.toList . view bigNatWords)
                  . strip

prop_implodeBytes = roundTrip (view pillWords) (view (from pillWords))

prop_explodeBytes = roundTrip (view (from pillWords) . VP.fromList)
                              (strip . VP.toList . view pillWords)
                  . strip

prop_packAtomSane = equiv (view (from pill)) dumbPackAtom . Pill . fromList
prop_unpackAtom   = roundTrip (view pill) (view (from pill))
prop_packAtom     = roundTrip (view (from pill)) (view pill) . Pill . strip

--------------------------------------------------------------------------------

main :: IO ()
main = $(defaultMainGenerator)
