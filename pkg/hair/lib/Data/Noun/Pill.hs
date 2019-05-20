{-# LANGUAGE MagicHash #-}

-- TODO Handle 32-bit architectures

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

import qualified Data.Primitive.ByteArray as Prim
import qualified Data.Vector.Primitive    as VP
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

wordArrToBigNat :: VP.Vector Word -> BigNat
wordArrToBigNat v@(VP.Vector off (I# len) (Prim.ByteArray buf)) =
  case VP.length v of
    0 -> zeroBigNat
    1 -> wordToBigNat (case VP.unsafeIndex v 0 of W# w -> w)
    n -> if off /= 0 then error "words2Nat: bad-vec" else
         byteArrayToBigNat# buf len

wordsToBigNat :: [Word] -> BigNat
wordsToBigNat = wordArrToBigNat . VP.fromList

bigNatToWords :: BigNat -> [Word]
bigNatToWords (BN# bArr) =
  stripTrailingZeros
    $ VP.toList
    $ VP.Vector 0 (I# (sizeofByteArray# bArr) `div` 8)
    $ Prim.ByteArray bArr

--------------------------------------------------------------------------------

naturalToBigNat :: Natural -> BigNat
naturalToBigNat (NatS# w)  = wordToBigNat w
naturalToBigNat (NatJ# bn) = bn

wordsToNatural :: [Word] -> Natural
wordsToNatural []  = 0
wordsToNatural [w] = fromIntegral w
wordsToNatural ws  = NatJ# (wordsToBigNat ws)

naturalToWords :: Natural -> [Word]
naturalToWords = bigNatToWords . naturalToBigNat

--------------------------------------------------------------------------------

dumbPackWord :: ByteString -> Word
dumbPackWord bs = go 0 0 (toList bs)
  where
    go acc i []     = acc
    go acc i (x:xs) = go (acc .|. shiftL (fromIntegral x) (8*i)) (i+1) xs

-- TODO This assumes 64-bit words
packWord :: ByteString -> Word
packWord buf = go 0 0 (toList buf)
  where
    go acc idx []     = acc
    go acc idx (x:xs) = go (acc .|. i idx (8*idx)) (idx+1) xs

    i :: Int -> Int -> Word
    i idx off = shiftL (fromIntegral $ BS.index buf idx) off

-- TODO This assumes 64-bit words
unpackWord :: Word -> ByteString
unpackWord wor = reverse $ fromList $ go 0 []
  where
    go i acc | i >= 8    = acc
    go i acc | otherwise = go (i+1) (fromIntegral (shiftR wor (i*8)) : acc)

--------------------------------------------------------------------------------

bytesToWords :: ByteString -> [Word]
bytesToWords = go []
  where
    go :: [Word] -> ByteString -> [Word]
    go acc buf | null buf  = reverse acc
    go acc buf | otherwise = go (packWord buf : acc) (drop 8 buf)

wordsToBytes :: [Word] -> ByteString
wordsToBytes = concat . fmap unpackWord

--------------------------------------------------------------------------------

dumbUnpackAtom :: ByteString -> Atom
dumbUnpackAtom bs = go 0 0 (toList bs)
  where
    go acc i []     = acc
    go acc i (x:xs) = go (acc .|. shiftL (fromIntegral x) (8*i)) (i+1) xs

unpackAtom :: ByteString -> Atom
unpackAtom = MkAtom . wordsToNatural . bytesToWords . stripTrailingZeros

loadFile :: FilePath -> IO Atom
loadFile = fmap unpackAtom . readFile

loadJam :: FilePath -> IO (Maybe Noun)
loadJam = fmap cue . loadFile

-- dumpJam :: FilePath -> Noun -> IO ()
-- dumpJam pat = writeFile pat . packAtom . jam

-- packAtom :: Atom -> ByteString
-- packAtom = undefined

dumpFlat :: Flat a => FilePath -> a -> IO ()
dumpFlat pat = writeFile pat . flat

loadFlat :: Flat a => FilePath -> IO (Either Text a)
loadFlat pat = do
  bs <- readFile pat
  pure $ mapLeft tshow $ unflat bs

data Pill = Brass | Ivory | Solid

tryPill :: Pill -> IO String
tryPill pill =
    loadJam pat <&> \case Nothing -> "nil"; Just (Atom _) -> "atom"; _ -> "cell"
  where
    pat = case pill of Brass -> "./bin/brass.pill"
                       Solid -> "./bin/solid.pill"
                       Ivory -> "./bin/ivory.pill"

-- Tests -----------------------------------------------------------------------

instance Arbitrary BigNat where
  arbitrary = naturalToBigNat <$> arbitrary

instance Show BigNat where
  show = show . NatJ#

roundTrip :: Eq a => (a -> b) -> (b -> a) -> (a -> Bool)
roundTrip f g x = x == g (f x)

equiv :: Eq b => (a -> b) -> (a -> b) -> (a -> Bool)
equiv f g x = f x == g x

check :: Atom -> Atom
check = toAtom . (id :: Integer -> Integer) . fromAtom

prop_packWord       = equiv packWord dumbPackWord . fromList
prop_unpackBigNat   = roundTrip bigNatToWords wordsToBigNat
prop_packBigNat     = roundTrip wordsToBigNat bigNatToWords . stripTrailingZeros
prop_unpackDumb     = equiv unpackAtom dumbUnpackAtom . fromList
prop_packUnpackWord = roundTrip unpackWord packWord
prop_explodeBytes   = roundTrip wordsToBytes bytesToWords

--------------------------------------------------------------------------------

main :: IO ()
main = $(defaultMainGenerator)

{-
/* u3i_bytes():
**
**   Copy `a` bytes from `b` to an LSB first atom.
*/
u3_noun
u3i_bytes(c3_w        a_w,
            const c3_y* b_y)
{
  /* Strip trailing zeroes.
  */
  while ( a_w && !b_y[a_w - 1] ) {
    a_w--;
  }

  /* Check for cat.
  */
  if ( a_w <= 4 ) {
    if ( !a_w ) {
      return 0;
    }
    else if ( a_w == 1 ) {
      return b_y[0];
    }
    else if ( a_w == 2 ) {
      return (b_y[0] | (b_y[1] << 8));
    }
    else if ( a_w == 3 ) {
      return (b_y[0] | (b_y[1] << 8) | (b_y[2] << 16));
    }
    else if ( (b_y[3] <= 0x7f) ) {
      return (b_y[0] | (b_y[1] << 8) | (b_y[2] << 16) | (b_y[3] << 24));
    }
  }

  /* Allocate, fill, return.
  */
  {
    c3_w        len_w = (a_w + 3) >> 2;
    c3_w*       nov_w = u3a_walloc((len_w + c3_wiseof(u3a_atom)));
    u3a_atom* nov_u = (void*)nov_w;

    nov_u->mug_w = 0;
    nov_u->len_w = len_w;

    /* Clear the words.
    */
    {
      c3_w i_w;

      for ( i_w=0; i_w < len_w; i_w++ ) {
        nov_u->buf_w[i_w] = 0;
      }
    }

    /* Fill the bytes.
    */
    {
      c3_w i_w;

      for ( i_w=0; i_w < a_w; i_w++ ) {
        nov_u->buf_w[i_w >> 2] |= (b_y[i_w] << ((i_w & 3) * 8));
      }
    }
    return u3a_to_pug(u3a_outa(nov_w));
  }
}
-}
