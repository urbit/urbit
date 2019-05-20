{-# LANGUAGE MagicHash #-}

module Data.Noun.Jam.Fast where

import ClassyPrelude
import Data.Noun
import Data.Noun.Atom
import Data.Noun.Poet
import Data.Bits
import Control.Lens
import Text.Printf
import GHC.Prim
import GHC.Word
import GHC.Natural

import Data.Map      (Map)
import Control.Monad (guard)

import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck

-- High-Performance Jam --------------------------------------------------------

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
