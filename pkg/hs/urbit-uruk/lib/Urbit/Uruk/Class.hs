{-# OPTIONS_GHC -Werror #-}

module Urbit.Uruk.Class where

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

instance Uruk p => Uruk (Either a p) where
  uApp x y = sequence $ uApp <$> x <*> y

  uJay p = pure (uJay p)

  uKay = pure uKay
  uEss = pure uEss
  uDee = pure uDee

  uBee = pure uBee
  uSea = pure uSea
  uEye = pure uEye

  uBen p = pure (uBen p)
  uSen p = pure (uSen p)
  uCen p = pure (uCen p)

  uNat n = pure (uNat n)
  uBol b = pure (uBol b)

  uCas = pure uCas
  uLef = pure uLef
  uRit = pure uRit
  uIff = pure uIff
  uSeq = pure uSeq
  uPak = pure uPak
  uZer = pure uZer
  uEql = pure uEql
  uInc = pure uInc
  uDec = pure uDec
  uFec = pure uFec
  uAdd = pure uAdd
  uSub = pure uSub
  uMul = pure uMul
  uFix = pure uFix
  uDed = pure uDed
  uUni = pure uUni
  uCon = pure uCon
  uCar = pure uCar
  uCdr = pure uCdr
