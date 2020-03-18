{-# LANGUAGE CPP #-}

{-|
    Atom implementation with fast conversions between bytestrings
    and atoms.
-}

module Urbit.Atom
  ( Atom
  , atomBytes
  , bytesAtom
  , atomWords
  , wordsAtom
  , utf8Atom
  , atomUtf8
  , atomUtf8Exn
  , atomUtf8Lenient
  )
where

import Prelude

import Data.ByteString       (ByteString)
import Data.Vector.Primitive (Vector)
import GHC.Natural           (Natural)

import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import qualified Data.Text.Encoding.Error as T

#if defined(__GHCJS__)
import qualified Urbit.Atom.Slow as Slow
#endif

import qualified Urbit.Atom.Fast as A


--------------------------------------------------------------------------------

type Atom = Natural


-- Choose Implementation Based on Platform -------------------------------------

{- |
  Convert an Atom to a bytestring. O(n), copies.

  My hand-rolled implementation is faster, but doesn't work on GHCJS. So,
  on GHCJS use GMP's `export` routine.

  TODO GMP's `export` routine also handles big endian machines, so use
  in that case too.
-}
atomBytes :: Atom -> ByteString
atomBytes =
#if defined(__GHCJS__)
  A.exportBytes
#else
  A.atomBytes
#endif

{- |
  Convert a bytestring to an Atom. O(n), copies.

  This always uses GMP's `export` routine, since it's portable and faster
  than my hand-rolled implementation.
-}
bytesAtom :: ByteString -> Atom
bytesAtom = A.importBytes

{- |
  Cast an atom to a vector. O(1), does not copy.

  My fast implementation doesn't work on GHCJS, so fallback to the naive
  implementation on that platform for now.
-}
atomWords :: Atom -> Vector Word
atomWords =
#if defined(__GHCJS__)
  Slow.atomWords
#else
  A.atomWords
#endif

{- |
  Cast a vector to an atom. O(1), does not copy unless given a slice,
  then O(n).

  My fast implementation doesn't work on GHCJS, so fallback to the naive
  implementation on that platform for now.
-}
wordsAtom :: Vector Word -> Atom
wordsAtom =
#if defined(__GHCJS__)
  Slow.wordsAtom
#else
  A.wordsAtom
#endif


-- String/Cord Conversion ------------------------------------------------------

-- | Encode a utf8-encoded atom from text.
utf8Atom :: T.Text -> Atom
utf8Atom = bytesAtom . T.encodeUtf8

-- | Interpret an atom as utf8 text.
atomUtf8 :: Atom -> Either T.UnicodeException T.Text
atomUtf8 = T.decodeUtf8' . atomBytes

-- | Interpret an atom as utf8 text, throwing an exception on bad unicode.
atomUtf8Exn :: Atom -> T.Text
atomUtf8Exn = T.decodeUtf8 . atomBytes

-- | Interpret an atom as utf8 text, replacing bad unicode characters.
atomUtf8Lenient :: Atom -> T.Text
atomUtf8Lenient = T.decodeUtf8With T.lenientDecode . atomBytes
