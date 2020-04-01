{-# OPTIONS_GHC -Werror #-}

module Urbit.Uruk.Class where

import ClassyPrelude

import Numeric.Natural  (Natural)
import Numeric.Positive (Positive)

--------------------------------------------------------------------------------

class Uruk p where
  uApp :: p -> p -> IO p

  uEss :: p
  uKay :: p
  uJay :: Positive -> p
  uDee :: p

  uBee :: p
  uSea :: p
  uEye :: p

  uBen :: Positive -> p
  uSen :: Positive -> p
  uCen :: Positive -> p

  uNat :: Natural -> p
  uBol :: Bool -> p

  uUni :: p
  uCon :: p
  uSeq :: p
  uCas :: p
  uFix :: p
  uIff :: p

  uGlobal :: Text -> Maybe p
  uGlobal = const Nothing

instance Uruk p => Uruk (Either a p) where
  uApp x y = sequence $ uApp <$> x <*> y

  uEss   = pure uEss
  uKay   = pure uKay
  uJay p = pure (uJay p)
  uDee   = pure uDee

  uBee = pure uBee
  uSea = pure uSea
  uEye = pure uEye

  uBen p = pure (uBen p)
  uSen p = pure (uSen p)
  uCen p = pure (uCen p)

  uNat n = pure (uNat n)
  uBol b = pure (uBol b)

  uUni = pure uUni
  uCon = pure uCon

  uSeq = pure uSeq
  uCas = pure uCas
  uFix = pure uFix
  uIff = pure uIff

  uGlobal = sequence . pure . uGlobal
