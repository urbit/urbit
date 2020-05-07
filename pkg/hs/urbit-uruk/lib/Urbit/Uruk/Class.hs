{-# OPTIONS_GHC -Wall -Werror #-}

module Urbit.Uruk.Class where

import ClassyPrelude

import Numeric.Natural  (Natural)
import Urbit.Pos        (Pos)
import Urbit.Atom       (utf8Atom)
import Urbit.Moon.Arity (Arity)

--------------------------------------------------------------------------------

class Show p => Uruk p where
  uApp :: p -> p -> IO p

  uEss :: p
  uKay :: p
  uJay :: Pos -> p
  uDee :: p

  uSen :: Pos -> p
  uBee :: Pos -> p
  uSea :: Pos -> p
  uEye :: Pos -> p

  uNat :: Natural -> p
  uBol :: Bool -> p

  uUni :: p
  uCon :: p
  uSeq :: p
  uLet :: p
  uCas :: p
  uFix :: p
  uIff :: p

  uArity :: p -> Maybe Arity

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

  uSen p = pure (uSen p)
  uBee p = pure (uBee p)
  uSea p = pure (uSea p)
  uEye p = pure (uEye p)

  uNat n = pure (uNat n)
  uBol b = pure (uBol b)

  uUni = pure uUni
  uCon = pure uCon

  uSeq = pure uSeq
  uLet = pure uLet
  uCas = pure uCas
  uFix = pure uFix
  uIff = pure uIff

  uArity (Left _)  = Nothing
  uArity (Right x) = uArity x

  uGlobal = sequence . pure . uGlobal
