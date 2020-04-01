{-# OPTIONS_GHC -Wall -Werror #-}

module Urbit.Moon.MoonToLambda (moonToLambda) where

import Bound
import ClassyPrelude
import GHC.Natural
import Urbit.Uruk.Class

import Urbit.Uruk.Bracket (Exp((:@)))

import qualified Urbit.Atom         as Atom
import qualified Urbit.Moon.AST     as M
import qualified Urbit.Uruk.Bracket as B


--------------------------------------------------------------------------------

{- |
    Compile Moon expressions to Enriched Lambda Calculus.
-}
moonToLambda :: forall p. Uruk p => M.Exp Text -> B.Exp () (Either Text p)
moonToLambda = twist . go
 where
  go :: M.Exp a -> B.Exp () (Either p a)
  go = \case
    M.App x y   -> go x :@ go y
    M.Var v     -> B.Var (Right v)
    M.Lam b     -> B.Lam () $ toScope $ fmap sequence $ go $ fromScope b
    M.Fix b     -> B.Var (Left uFix) :@ go (M.Lam b)
    M.Sig       -> pri uUni
    M.Lit n     -> pri (uNat n)
    M.Bol b     -> pri (uBol b)
    M.Str n     -> pri (uNat (Atom.utf8Atom n))
    M.Con a b   -> pri uCon :@ go a :@ go b
    M.Cas x l r -> pri uCas :@ go x :@ go (M.Lam l) :@ go (M.Lam r)
    M.Iff c t e -> pri uIff :@ go c :@ lam t :@ lam e
    M.Jet n t b -> jay n :@ tag t :@ go b

  twist :: Functor f => f (Either a b) -> f (Either b a)
  twist = fmap (either Right Left)

  pri :: p -> B.Exp b (Either p a)
  pri = B.Var . Left

  jay :: Natural -> B.Exp () (Either p a)
  jay = pri . uJay . fromIntegral

  tag :: Text -> B.Exp b (Either p a)
  tag = pri . uNat . Atom.utf8Atom

  lam :: M.Exp a -> B.Exp () (Either p a)
  lam = go . M.Lam . abstract (const Nothing)
