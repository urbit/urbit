{-# OPTIONS_GHC -Werror #-}

module Uruk.Class where

import ClassyPrelude

import Numeric.Natural  (Natural)
import Numeric.Positive (Positive)

--------------------------------------------------------------------------------

class Uruk p where
  uApp :: p -> p -> IO p

  uJay :: Positive -> p
  uKay :: p
  uEss :: p
  uDee :: p

  uBee :: p
  uSea :: p
  uEye :: p

  uBen :: Positive -> p
  uSen :: Positive -> p
  uCen :: Positive -> p

  uNat :: Natural -> p
  uBol :: Bool -> p

  uCas :: p
  uLef :: p
  uRit :: p
  uIff :: p
  uSeq :: p
  uPak :: p
  uZer :: p
  uEql :: p
  uInc :: p
  uDec :: p
  uFec :: p
  uAdd :: p
  uSub :: p
  uMul :: p
  uFix :: p
  uDed :: p
  uUni :: p
  uCon :: p
  uCar :: p
  uCdr :: p
