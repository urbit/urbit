{-|
    Atom implementation with fast conversions between bytestrings
    and atoms.
-}

module Urbit.Atom
    ( Atom
    , atomBytes, bytesAtom
    , atomWords, wordsAtom
    , utf8Atom, atomUtf8, atomUtf8Exn, atomUtf8Lenient
    ) where

import Prelude

import Data.ByteString       (ByteString)
import Data.Vector.Primitive (Vector)
import GHC.Natural           (Natural)

import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import qualified Data.Text.Encoding.Error as T
import qualified Urbit.Atom.Internal      as I


--------------------------------------------------------------------------------

type Atom = Natural

--------------------------------------------------------------------------------

-- | Cast an atom to a vector. Does not copy.
atomWords :: Atom → Vector Word
atomWords = I.natWords

-- | Cast a vector to an atom. Does not copy unless given a slice.
wordsAtom :: Vector Word → Atom
wordsAtom = I.wordsNat

-- | Dump an atom to a bytestring.
atomBytes ∷ Atom → ByteString
atomBytes = I.pillBytes . I.natPill

-- | Load a bytestring into an atom.
bytesAtom ∷ ByteString → Atom
bytesAtom = I.pillNat . I.bytesPill

-- | Encode a utf8-encoded atom from text.
utf8Atom ∷ T.Text → Atom
utf8Atom = bytesAtom . T.encodeUtf8

-- | Interpret an atom as utf8 text.
atomUtf8 ∷ Atom → Either T.UnicodeException T.Text
atomUtf8 = T.decodeUtf8' . atomBytes

-- | Interpret an atom as utf8 text, throwing an exception on bad unicode.
atomUtf8Exn ∷ Atom → T.Text
atomUtf8Exn = T.decodeUtf8 . atomBytes

-- | Interpret an atom as utf8 text, replacing bad unicode characters.
atomUtf8Lenient ∷ Atom → T.Text
atomUtf8Lenient = T.decodeUtf8With T.lenientDecode . atomBytes
