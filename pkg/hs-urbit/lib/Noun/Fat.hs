{-|
    Nouns with Pre-Computed Hash for each node.
-}

{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -fwarn-unused-binds -fwarn-unused-imports #-}

module Noun.Fat ( FatNoun(..)
                , fatHash, fatCell, fatAtom
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

data FatNoun
    = FatCell {-# UNPACK #-} !Int
              !FatNoun
              !FatNoun
    | FatWord {-# UNPACK #-} !Word
    | FatAtom {-# UNPACK #-} !Int
              {-# UNPACK #-} !BigNat


--------------------------------------------------------------------------------

instance Hashable FatNoun where
  hash = fatHash
  {-# INLINE hash #-}
  hashWithSalt = defaultHashWithSalt
  {-# INLINE hashWithSalt #-}

instance Eq FatNoun where
  (==) x y =
    case reallyUnsafePtrEquality# x y of
      1# -> True
      _  -> case (x, y) of
              (FatWord w1,       FatWord w2      ) ->
                  w1==w2
              (FatAtom x1 a1,    FatAtom x2 a2   ) ->
                  x1==x2 && a1==a2
              (FatCell x1 h1 t1, FatCell x2 h2 t2) ->
                  x1==x2 && h1==h2 && t1==t2
              (_,                _               ) ->
                  False
  {-# INLINE (==) #-}


--------------------------------------------------------------------------------

{-# INLINE fatHash #-}
fatHash :: FatNoun -> Int
fatHash = \case
  FatCell h _ _ -> h
  FatAtom h _   -> h
  FatWord w     -> hash w

{-# INLINE fatAtom #-}
fatAtom :: Atom -> FatNoun
fatAtom = \case
  MkAtom   (NatS# wd) -> FatWord (W# wd)
  MkAtom n@(NatJ# bn) -> FatAtom (hash bn) bn

{-# INLINE fatCell #-}
fatCell :: FatNoun -> FatNoun -> FatNoun
fatCell h t = FatCell has h t
  where
    has = fatHash h `combine` fatHash t

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
          FatAtom _ a   -> Atom (MkAtom $ NatJ# a)
          FatCell _ h t -> Cell (go h) (go t)
          FatWord w     -> Atom (fromIntegral w)


-- Stolen from Hashable Library ------------------------------------------------

{-# INLINE combine #-}
combine :: Int -> Int -> Int
combine h1 h2 = (h1 * 16777619) `xor` h2

{-# INLINE defaultHashWithSalt #-}
defaultHashWithSalt :: Hashable a => Int -> a -> Int
defaultHashWithSalt salt x = salt `combine` hash x
