{-# OPTIONS_GHC -Wall -Werror #-}

module Urbit.Pos (Pos(..)) where

import ClassyPrelude

import Numeric.Positive


--------------------------------------------------------------------------------

newtype Pos = MkPos { unPos :: Positive }
 deriving newtype (Eq, Ord, Show, Num, Enum, Real, Integral)

instance Hashable Pos where
  hashWithSalt i (MkPos p) = hashWithSalt i (toInteger p)

instance NFData Pos where
  rnf (MkPos !_) = ()
