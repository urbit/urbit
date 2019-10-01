module Untyped.Hoon where

import ClassyPrelude

import Bound.Name
import Control.Monad.State

import Noun
import Untyped.Core

data Hoon a
  = HVar a
  | HCons (Hoon a) (Hoon a)
  | BarCen (Cases a)
  | BarHep a a (Hoon a) (Hoon a)
  | BarTis a (Hoon a)
  | CenBar a (Hoon a)
  | CenGar (Hoon a) (Hoon a)
  | CenGal (Hoon a) (Hoon a)
  | CenKet (Hoon a) (Hoon a) (Hoon a)
  | CenTar [Hoon a]
  | TisFas a (Hoon a)
  | DotLus (Hoon a)
  | DotTis (Hoon a) (Hoon a)
  | WutCol (Hoon a) (Hoon a) (Hoon a)
  | WutHep (Hoon a) (Cases a)
  | ZapZap

type Cases a = [(Pat, Hoon a)]

data Pat
  = Exact Noun
  | Wild

desugar :: Eq a => Hoon a -> Exp a
desugar = go
  where
    go = \case
      HVar v -> Var v
      HCons h j -> Cel (go h) (go j)
      BarCen cs -> undefined --Lam $ Scope $ go $ WutHep (Var (B ())) 
      -- CenBar cs -> Fix $ Scope
      WutHep h cs -> undefined

branch :: (Hoon b -> Exp a) -> Exp a -> Cases b -> Exp a
branch go e = foldr f Zap
  where
   f c acc = case c of
     (Exact n, h) -> Ift (Eql e (con n)) (go h) acc
     (Wild,    h) -> go h

