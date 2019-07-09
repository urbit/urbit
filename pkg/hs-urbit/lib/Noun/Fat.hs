{-|
    Nouns with Pre-Computed Hash for each node.
-}

{-# LANGUAGE MagicHash, Strict #-}
{-# OPTIONS_GHC -fwarn-unused-binds -fwarn-unused-imports #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Noun.Fat ( FatNoun(..), FatAtom(..)
                , fatSize
                , fatCell, fatAtom
                , toFatNoun, fromFatNoun
                ) where

import ClassyPrelude hiding (hash)

import Data.Bits                 (xor)
import Data.Hashable             (hash)
import GHC.Integer.GMP.Internals (BigNat)
import GHC.Natural               (Natural(NatS#, NatJ#))
import GHC.Prim                  (reallyUnsafePtrEquality#)
import GHC.Word                  (Word(W#))
import Noun.Atom                 (Atom(MkAtom))
import Noun                      (Noun(Atom, Cell))


--------------------------------------------------------------------------------

data FatAtom
    = FatWord !Word
    | FatBigN !Int !BigNat

data FatNoun
    = FatCell !Int !Word !FatNoun !FatNoun
    | FatAtom !FatAtom


--------------------------------------------------------------------------------

instance Hashable FatAtom where
  hash = atomHash
  {-# INLINE hash #-}
  hashWithSalt = defaultHashWithSalt
  {-# INLINE hashWithSalt #-}

instance Hashable FatNoun where
  hash = nounHash
  {-# INLINE hash #-}
  hashWithSalt = defaultHashWithSalt
  {-# INLINE hashWithSalt #-}

instance Eq FatAtom where
  (==) x y =
    case reallyUnsafePtrEquality# x y of
      1# -> True
      _  -> case (x, y) of
              (FatWord w1,    FatWord w2      ) -> w1==w2
              (FatBigN x1 a1, FatBigN x2 a2   ) -> x1==x2 && a1==a2
              _                                 -> False
  {-# INLINE (==) #-}

instance Eq FatNoun where
  (==) x y =
    case reallyUnsafePtrEquality# x y of
      1# -> True
      _  -> case (x, y) of
              (FatAtom a1, FatAtom a2) ->
                  a1 == a2
              (FatCell x1 s1 h1 t1, FatCell x2 s2 h2 t2) ->
                  s1==s2 && x1==x2 && h1==h2 && t1==t2
              _ ->
                  False
  {-# INLINE (==) #-}


--------------------------------------------------------------------------------

{-# INLINE fatSize #-}
fatSize :: FatNoun -> Word
fatSize = \case
  FatCell _ s _ _ -> s
  _               -> 1

{-# INLINE atomHash #-}
atomHash :: FatAtom -> Int
atomHash = \case
  FatBigN h _ -> h
  FatWord w   -> hash w

{-# INLINE nounHash #-}
nounHash :: FatNoun -> Int
nounHash = \case
  FatCell h _ _ _ -> h
  FatAtom a       -> hash a

{-# INLINE fatAtom #-}
fatAtom :: Atom -> FatNoun
fatAtom = \case
  MkAtom   (NatS# wd) -> FatAtom $ FatWord (W# wd)
  MkAtom n@(NatJ# bn) -> FatAtom $ FatBigN (hash bn) bn

{-# INLINE fatCell #-}
fatCell :: FatNoun -> FatNoun -> FatNoun
fatCell h t = FatCell has siz h t
  where
    siz = fatSize h + fatSize t
    has = nounHash h `combine` nounHash t

{-# INLINE toFatNoun #-}
toFatNoun :: Noun -> FatNoun
toFatNoun = go
  where
    go (Atom a)   = fatAtom a
    go (Cell h t) = fatCell (go h) (go t)

{-# INLINE fromFatNoun #-}
fromFatNoun :: FatNoun -> Noun
fromFatNoun = go
  where go = \case
          FatCell _ _ h t       -> Cell (go h) (go t)
          FatAtom (FatBigN _ a) -> Atom (MkAtom $ NatJ# a)
          FatAtom (FatWord w)   -> Atom (fromIntegral w)


-- Stolen from Hashable Library ------------------------------------------------

{-# INLINE combine #-}
combine :: Int -> Int -> Int
combine h1 h2 = (h1 * 16777619) `xor` h2

{-# INLINE defaultHashWithSalt #-}
defaultHashWithSalt :: Hashable a => Int -> a -> Int
defaultHashWithSalt salt x = salt `combine` hash x
