module Untyped.Hoon where

import ClassyPrelude

import Bound
import Bound.Name

import SimpleNoun
import Untyped.Core

data Hoon a
  = HVar a
  | HAtom Atom
  | HCons (Hoon a) (Hoon a)
  | BarCen (Cases a)
  | BarHep a a (Hoon a) (Hoon a)
  | BarTis a (Hoon a)
  | CenDot (Hoon a) (Hoon a)
  | CenHep (Hoon a) (Hoon a)
--  | CenKet (Hoon a) (Hoon a) (Hoon a)
--  | CenTar [Hoon a]
  | TisFas a (Hoon a) (Hoon a)
  | DotDot a (Hoon a)
  | DotLus (Hoon a)
  | DotTis (Hoon a) (Hoon a)
  | SigFas Atom (Hoon a)
  | WutBar (Hoon a) (Hoon a)
  | WutCol (Hoon a) (Hoon a) (Hoon a)
  | WutHep (Hoon a) (Cases a)
  | WutKet (Hoon a) (Hoon a) (Hoon a)
  | WutPam (Hoon a) (Hoon a)
  | WutPat (Hoon a) (Hoon a) (Hoon a)
  | ZapZap
  deriving (Functor)

deriving instance Show a => Show (Hoon a)

type Cases a = [(Pat, Hoon a)]

data Pat
  = Exact Noun
  | Wild
  deriving (Show)

desugar :: Eq a => Hoon a -> Exp a
desugar = go
  where
    go = \case
      HVar v         -> Var v
      HAtom a        -> Atm a
      HCons h j      -> Cel (go h) (go j)
      BarCen cs      -> Lam $ Scope $ branch (Var . F . go) (Var (B ())) cs
      BarHep r s i h -> go $ CenDot i $ DotDot r $ BarTis s $ h
      BarTis v h     -> lam v (go h)
      CenDot h j     -> App (go j) (go h)
      CenHep h j     -> App (go h) (go j)
      TisFas v h j   -> ledt v (go h) (go j)
      DotDot v h     -> fix v (go h)
      DotLus h       -> Suc (go h)
      DotTis h j     -> Eql (go h) (go j)
      SigFas a h     -> Jet a (go h)
      WutBar h j     -> Ift (go h) (Atm 0) (go j)
      WutCol h j k   -> Ift (go h) (go j) (go k)
      -- or branch go (go h) cs
      WutHep h cs    -> Let (go h) $ Scope $ branch (Var . F . go) (Var (B ())) cs
      WutKet h j k   -> Ift (IsC (go h)) (go j) (go k)
      WutPam h j     -> Ift (go h) (go j) (Atm 1)
      WutPat h j k   -> go $ WutKet h k j
      ZapZap         -> Zap

branch :: (Hoon b -> Exp a) -> Exp a -> Cases b -> Exp a
branch go e = foldr f Zap
  where
   f c acc = case c of
     (Exact n, h) -> Ift (Eql e (con n)) (go h) acc
     (Wild,    h) -> go h

