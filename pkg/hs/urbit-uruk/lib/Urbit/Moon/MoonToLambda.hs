{-# OPTIONS_GHC -Wall -Werror #-}

module Urbit.Moon.MoonToLambda (moonToLambda) where

import Bound
import ClassyPrelude
import GHC.Natural
import Urbit.Uruk.Class

import Urbit.Moon.Bracket (Exp((:@)))

import qualified Urbit.Atom         as Atom
import qualified Urbit.Moon.AST     as M
import qualified Urbit.Moon.Bracket as B


--------------------------------------------------------------------------------

{- |
    Compile Moon expressions to Enriched Lambda Calculus.
-}
moonToLambda :: forall p v. Uruk p => M.Exp v -> B.Exp p () v
moonToLambda = go
 where
  go :: M.Exp a -> B.Exp p () a
  go = \case
    M.App x y   -> go x :@ go y
    M.Var v     -> B.Var v
    M.Lam b     -> B.Lam () $ toScope $ go $ fromScope b
    M.Fix b     -> B.Pri uFix :@ go (M.Lam b)
    M.Sig       -> B.Pri uUni
    M.Lit n     -> B.Pri (uNat n)
    M.Bol b     -> B.Pri (uBol b)
    M.Str n     -> B.Pri (uNat (Atom.utf8Atom n))
    M.Con a b   -> B.Pri uCon :@ go a :@ go b
    M.Cas x l r -> B.Pri uCas :@ go x :@ go (M.Lam l) :@ go (M.Lam r)
    M.Let x k   -> B.Pri uLet :@ go x :@ go (M.Lam k)
    M.Iff c t e -> B.Pri uIff :@ go c :@ lam t :@ lam e
    M.Jet n t b -> jay n :@ tag t :@ go b

  jay :: Natural -> B.Exp p () a
  jay = B.Pri . uJay . fromIntegral

  tag :: Text -> B.Exp p b a
  tag = B.Pri . uNat . Atom.utf8Atom

  lam :: M.Exp a -> B.Exp p () a
  lam = go . M.Lam . abstract (const Nothing)
