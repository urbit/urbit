{-# LANGUAGE CPP #-}

module Urbit.Atom.Slow
  ( wordsNat
  , natWords
  , natBytes
  , bytesNat
  )
where

import Numeric.Natural
import Prelude
import Data.Bits
import Data.Word

import Data.ByteString (ByteString)
import Data.Vector.Primitive (Vector)

import qualified Data.ByteString       as BS
import qualified Data.Vector.Primitive as VP


-- Setup BIT and BYT macros. ---------------------------------------------------

#include <MachDeps.h>

#if WORD_SIZE_IN_BITS == 64
#define BIT 64
#define BYT 8
#elif WORD_SIZE_IN_BITS == 32
#define BIT 32
#define BYT 4
#else
#error WORD_SIZE_IN_BITS must be either 32 or 64
#endif


--------------------------------------------------------------------------------

{-|
  Natural number to LSB-ByteString.
-}
natBytes :: Natural -> ByteString
natBytes = BS.pack . go []
 where
  go acc 0 = reverse acc
  go acc n = go (fromIntegral n : acc) (shiftR n 8)

{-|
  LSB-first ByteString to Natural number.
-}
bytesNat :: ByteString -> Natural
bytesNat = BS.foldr' go 0
 where
  go :: Word8 -> Natural -> Natural
  go byt acc = shiftL acc 8 .|. fromIntegral byt


--------------------------------------------------------------------------------

{-|
  LSW-first Word Vector to Natural number.
-}
wordsNat :: Vector Word -> Natural
wordsNat = VP.foldr' go 0
 where
  go :: Word -> Natural -> Natural
  go wor acc = shiftL acc BIT .|. fromIntegral wor

{-|
  Natural number to LSW-first Word Vector.
-}
natWords :: Natural -> Vector Word
natWords = VP.fromList . go []
 where
  go acc 0 = reverse acc
  go acc n = go (fromIntegral n : acc) (shiftR n BIT)
