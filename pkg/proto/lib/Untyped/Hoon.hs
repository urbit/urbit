module Untyped.Hoon where

import ClassyPrelude

import Bound
import Bound.Name

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
--  | CenKet (Hoon a) (Hoon a) (Hoon a)
--  | CenTar [Hoon a]
  | TisFas a (Hoon a) (Hoon a)
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
      HVar v         -> Var v
      HCons h j      -> Cel (go h) (go j)
      BarCen cs      -> Lam $ Scope $ branch (Var . F . go) (Var (B ())) cs
      BarHep r s i h -> go $ CenGar i $ CenBar r $ BarTis s $ h
      BarTis v h     -> lam v (go h)
      CenBar v h     -> fix v (go h)
      CenGar h j     -> App (go j) (go h)
      CenGal h j     -> App (go h) (go j)
      TisFas v h j   -> ledt v (go h) (go j)
      DotLus h       -> Suc (go h)
      DotTis h j     -> Eql (go h) (go j)
      WutCol h j k   -> Ift (go h) (go j) (go k)
      -- or branch go (go h) cs
      WutHep h cs    -> Let (go h) $ Scope $ branch (Var . F . go) (Var (B ())) cs
      ZapZap         -> Zap

branch :: (Hoon b -> Exp a) -> Exp a -> Cases b -> Exp a
branch go e = foldr f Zap
  where
   f c acc = case c of
     (Exact n, h) -> Ift (Eql e (con n)) (go h) acc
     (Wild,    h) -> go h

