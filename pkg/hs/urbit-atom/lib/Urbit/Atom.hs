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

#if defined(__GHCJS__)
import Urbit.Atom.Slow       (atomBytes, bytesAtom)
#else
import Urbit.Atom.Fast       (atomBytes, bytesAtom)
#endif

import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import qualified Data.Text.Encoding.Error as T

#if defined(__GHCJS__)
import qualified Urbit.Atom.Slow as A
#else
import qualified Urbit.Atom.Fast as A
#endif


--------------------------------------------------------------------------------

type Atom = Natural

--------------------------------------------------------------------------------

-- | Cast an atom to a vector. Does not copy.
atomWords :: Atom -> Vector Word
atomWords = A.atomWords

-- | Cast a vector to an atom. Does not copy unless given a slice.
wordsAtom :: Vector Word -> Atom
wordsAtom = A.wordsAtom

-- | Encode a utf8-encoded atom from text.
utf8Atom :: T.Text -> Atom
utf8Atom = A.bytesAtom . T.encodeUtf8

-- | Interpret an atom as utf8 text.
atomUtf8 :: Atom -> Either T.UnicodeException T.Text
atomUtf8 = T.decodeUtf8' . atomBytes

-- | Interpret an atom as utf8 text, throwing an exception on bad unicode.
atomUtf8Exn :: Atom -> T.Text
atomUtf8Exn = T.decodeUtf8 . atomBytes

-- | Interpret an atom as utf8 text, replacing bad unicode characters.
atomUtf8Lenient :: Atom -> T.Text
atomUtf8Lenient = T.decodeUtf8With T.lenientDecode . atomBytes
