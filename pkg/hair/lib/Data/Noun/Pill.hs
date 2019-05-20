{-# LANGUAGE MagicHash #-}

module Data.Noun.Pill where

import ClassyPrelude
import Data.Noun
import Data.Noun.Atom
import Data.Noun.Jam
import Data.Flat
import Control.Monad.Except
import Control.Lens
import Data.Either.Extra (mapLeft)
import GHC.Natural
import Data.Bits
import GHC.Integer.GMP.Internals

import qualified Data.Primitive.ByteArray as Prim
import qualified Data.Vector.Primitive    as VP
import qualified Data.ByteString          as BS

--------------------------------------------------------------------------------

stripTrailingZeros :: ByteString -> ByteString
stripTrailingZeros buf = BS.take (len - go 0 (len - 1)) buf
  where
    len = length buf
    go n i | i < 0               = n
           | 0 == BS.index buf i = go (n+1) (i-1)
           | otherwise           = n

unpackWord :: ByteString -> Word
unpackWord buf =
  case length buf of
    0 -> 0
    1 -> i 0 0
    2 -> i 0 0 .|. i 1 8
    3 -> i 0 0 .|. i 1 8 .|. i 2 16
    n -> i 0 0 .|. i 1 8 .|. i 2 16 .|. i 3 24
  where
    i :: Int -> Int -> Word
    i idx off = shiftL (fromIntegral $ BS.index buf idx) off

words2Nat :: [Word] -> Natural
words2Nat []  = 0
words2Nat [w] = fromIntegral w
words2Nat ws =
    if off /= 0 then error "words2Nat bad vec" else
    NatJ# (BN# buf)
  where
    VP.Vector off len (Prim.ByteArray buf) = VP.fromList ws

unpackWords :: ByteString -> [Word]
unpackWords =
    \case buf | length buf <= 4 -> [unpackWord buf]
              | otherwise       -> go [] buf
  where
    go :: [Word] -> ByteString -> [Word]
    go acc buf | null buf  = reverse acc
    go acc buf | otherwise = go (unpackWord buf : acc) (BS.drop 4 buf)

unpackAtom :: ByteString -> Atom
unpackAtom = MkAtom . words2Nat . unpackWords . stripTrailingZeros

loadFile :: FilePath -> IO Atom
loadFile = fmap unpackAtom . readFile

loadJam :: FilePath -> IO (Maybe Noun)
loadJam = fmap cue . loadFile

-- dumpJam :: FilePath -> Noun -> IO ()
-- dumpJam pat = writeFile pat . packAtom . jam

-- packWord :: Word -> ByteString
-- packWord buf = undefined

-- packAtom :: Atom -> ByteString
-- packAtom = undefined

dumpFlat :: Flat a => FilePath -> a -> IO ()
dumpFlat pat = writeFile pat . flat

loadFlat :: Flat a => FilePath -> IO (Either Text a)
loadFlat pat = do
  bs <- readFile pat
  pure $ mapLeft tshow $ unflat bs

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
