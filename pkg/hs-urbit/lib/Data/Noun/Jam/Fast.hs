{-# LANGUAGE MagicHash #-}

module Data.Noun.Jam.Fast where

import ClassyPrelude
import Data.Noun
import Data.Noun.Atom
import Data.Noun.Poet
import Data.Bits hiding (Bits)
import Control.Lens
import Text.Printf
import GHC.Prim
import GHC.Word
import GHC.Natural
import Foreign.Ptr
import Foreign.Storable (peek)
import Data.Noun.Jam.Get

import Data.Map      (Map)
import Control.Monad (guard)

import Test.Tasty
import Test.Tasty.TH
import qualified Test.Tasty.QuickCheck as QC
import Test.QuickCheck hiding ((.&.))

import qualified Data.HashTable.IO as H


-- Pre-compute the bit-width of a jammed noun. ---------------------------------

jamSz :: Noun -> Word
jamSz = fst . go 0 mempty
  where
    insertNoun :: Noun -> Word -> Map Noun Word -> Map Noun Word
    insertNoun n i tbl = lookup n tbl
                       & maybe tbl (const $ insertMap n i tbl)

    go :: Word -> Map Noun Word -> Noun -> (Word, Map Noun Word)
    go off oldTbl noun =
      let tbl = insertNoun noun off oldTbl in
      case lookup noun oldTbl of
        Nothing ->
          case noun of
            Atom atm ->
              (1 + W# (matSz# atm), tbl)
            Cell l r ->
              let (lSz, tbl) = go (2+off)     tbl l in
              let (rSz, tbl) = go (2+off+lSz) tbl r in
              (2 + lSz + rSz, tbl)
        Just (W# ref) ->
          let refSz = W# (wordBitWidth# ref) in
          case noun of
            Atom atm ->
              let worSz = W# (matSz# atm) in
              if worSz > refSz
              then (refSz, oldTbl)
              else (1 + worSz, tbl)
            Cell _ _ ->
              (refSz, oldTbl)

    matSz# :: Atom -> Word#
    matSz# 0 = 1##
    matSz# a = preW `plusWord#` preW `plusWord#` atmW
      where
        atmW = atomBitWidth# a
        preW = wordBitWidth# atmW

    refSz# :: Word# -> Word#
    refSz# w = 2## `plusWord#` (matSz# (MkAtom (NatS# w)))

    nounSz# :: Noun -> Word#
    nounSz# (Atom a)   = 1## `plusWord#` (matSz# a)
    nounSz# (Cell l r) = 2## `plusWord#` (nounSz# l) `plusWord#` (nounSz# r)

--------------------------------------------------------------------------------

jamFast :: Noun -> Vector Word64
jamFast n = undefined

bitsToAtom :: Bits -> Atom
bitsToAtom = undefined


-- Fast Cue --------------------------------------------------------------------

{-
    Get the exponent-prefix of an atom:

    - Peek at the next word.
    - Calculate the number of least-significant bits in that word (there's
      a primitive for this).
    - Advance by that number of bits.
    - Return the number of bits
-}
dExp :: Get Word
dExp = do
  W# w <- peekWord
  let res = W# (ctz# w)
  advance res
  pure res

dAtomLen :: Get Word
dAtomLen = do
  e <- dExp
  p <- dWordBits (e-1)
  pure (2^e .|. p)

dRef :: Get Word
dRef = dAtomLen >>= dWordBits

dAtom :: Get Atom
dAtom = do
  n <- dAtomLen
  b <- dBits n
  pure (bitsToAtom b)

dCell :: Get Noun
dCell = Cell <$> dNoun <*> dNoun

{-|
    Get a Noun.

    - Get a bit
    - If it's zero, get an atom.
    - Otherwise, get another bit.
    - If it's zero, get a cell.
    - If it's one, get an atom.
-}
dNoun :: Get Noun
dNoun = do
  p <- getPos

  let yield r = insRef p r >> pure r

  dBit >>= \case
    False -> (Atom <$> dAtom) >>= yield
    True  -> dBit >>= \case
      False -> dCell >>= yield
      True  -> dRef >>= getRef

{-
    TODO Count leading zero bits.

    Read a 64 bit word from the buffer and get the number of leading
    zeros in that word. This works as long as no atom is larger than
    2 zettabytes.

    - TODO Need to handle the edge-case where there are less than 64 bits
      remaining in the buffer. Those extra bytes need to be zeros. One way
      to handle this might be to add a zero word to the end of the buffer,
      but that would require a re-alloc. Probably the right way is to
      write new `peek` primitives that handle this case.

    - TODO Error out if we hit the end *and* the word is all zeros.

    Alright, let's pseudo-code this out:

      Grab the next 64 bits. Pill files are always LSB-first
-}
