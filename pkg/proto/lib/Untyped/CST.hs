module Untyped.CST where

import ClassyPrelude
import Prelude (foldr1)

import SimpleNoun
import qualified Noun as N
import qualified Untyped.Hoon as H
import Untyped.Parser  -- remove after we've moved the CST type

hone :: CST -> H.Hoon Sym
hone = go
  where
    go = \case 
      WutCol c d e -> H.WutCol (go c) (go d) (go e)
      WutPat c d e -> H.WutPat (go c) (go d) (go e)
      WutKet c d e -> H.WutKet (go c) (go d) (go e)
      WutPam cs -> foldr H.WutPam (H.HAtom 0) $ map go cs
      WutBar cs -> foldr H.WutBar (H.HAtom 1) $ map go cs
      WutHep c pcs -> H.WutHep (go c) (map tr pcs)
      TisFas s c d -> H.TisFas s (go c) (go d)
      ColHep c d -> H.HCons (go c) (go d)
      ColLus{} -> error "hone: offensive rune :+ -- use :*"
      ColKet{} -> error "hone: offensive rune :^ -- use :*"
      ColTar [] -> error "hone: empty :*"
      ColTar cs -> foldr1 H.HCons $ map go cs
      ColSig cs -> foldr H.HCons (H.HAtom 0) $ map go cs
      BarTis s c -> H.BarTis s (go c)
      BarHep r v i c -> H.BarHep r v (go i) (go c)
      BarCen pcs -> H.BarCen (map tr pcs)
      CenHep c d -> H.CenHep (go c) (go d)
      CenDot c d -> H.CenDot (go c) (go d)
      DotDot s c -> H.DotDot s (go c)
      SigFas (go -> H.HAtom a) c -> H.SigFas a (go c)
      SigFas{} -> error "hone: invalid ~/ tag"
      ZapZap -> H.ZapZap
      Tupl cs -> go (ColTar cs)
      Var s -> H.HVar s
      Atom a -> H.HAtom a
      Tag tx -> H.HAtom (textToAtom tx)
      Cord tx -> H.HAtom (textToAtom tx)
      Tape tx -> undefined
      Incr c -> H.DotLus (go c)
      IncrIrr c -> H.DotLus (go c)
      AppIrr c d -> H.CenHep (go c) (go d)
      IsEq c d -> H.DotTis (go c) (go d)
      IsEqIrr c d -> H.DotTis (go c) (go d)
      Pam -> H.HAtom 0
      Bar -> H.HAtom 1
      Yes -> H.HAtom 0
      No -> H.HAtom 1
      Sig -> H.HAtom 0

    tr (PatTar, c) = (H.Wild, go c)
    tr (PatTag s, c) = (H.Exact (A $ textToAtom s), go c)


textToAtom :: Text -> Atom
textToAtom t = case N.textToUtf8Atom t of
  N.A a -> a
