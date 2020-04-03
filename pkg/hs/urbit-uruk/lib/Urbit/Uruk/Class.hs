{-# OPTIONS_GHC -Werror #-}

module Urbit.Uruk.Class where

import ClassyPrelude

import Numeric.Natural  (Natural)
import Numeric.Positive (Positive)
import Urbit.Atom       (utf8Atom)

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
  uYet :: Positive -> p

  uNat :: Natural -> p
  uBol :: Bool -> p

  uUni :: p
  uCon :: p
  uSeq :: p
  uCas :: p
  uFix :: p
  uIff :: p

  uArity :: p -> Int

  uGlobal :: Text -> Maybe p
  uGlobal = const Nothing

mkAtom :: Uruk p => Either Text p -> p
mkAtom (Left t)  = uNat (utf8Atom t)
mkAtom (Right x) = x

instance Uruk p => Uruk (Either Text p) where
  uApp x y = do
    res <- uApp (mkAtom x) (mkAtom y)
    pure (Right res)

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
  uYet p = pure (uYet p)

  uNat n = pure (uNat n)
  uBol b = pure (uBol b)

  uUni = pure uUni
  uCon = pure uCon

  uSeq = pure uSeq
  uCas = pure uCas
  uFix = pure uFix
  uIff = pure uIff

  uArity (Left _)  = 1 -- Free
  uArity (Right x) = uArity x

  uGlobal = sequence . pure . uGlobal
