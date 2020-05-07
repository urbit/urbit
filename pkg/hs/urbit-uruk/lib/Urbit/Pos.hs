{-# OPTIONS_GHC -Wall -Werror #-}

module Urbit.Pos (Pos(..)) where

import ClassyPrelude

import Numeric.Positive (Positive)


--------------------------------------------------------------------------------

newtype Pos = MkPos { unPos :: Positive }
 deriving newtype (Eq, Ord, Show, Num, Enum, Real, Integral)

instance NFData Pos where
  rnf (MkPos !_) = ()
