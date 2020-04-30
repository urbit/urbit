module Urlicht.Hoon where

import ClassyPrelude

import Bound
import Data.Deriving (deriveEq1, deriveOrd1, deriveRead1, deriveShow1)
import Numeric.Natural

import Urlicht.Meta

type Atom = Natural

data Hoon a
  = Var a
  | Met Meta
  -- irregular forms
  | Hax
  | Fun (Hoon a) (Scope () Hoon a)
  | Cel (Hoon a) (Scope () Hoon a)
  | Wut (Set Atom)
  | Pat
  --
  | Lam (Hoon a) (Scope () Hoon a)
  | Cns (Hoon a) (Hoon a)
  | Nat Atom
  --
  | App (Hoon a) (Hoon a)
  | Hed (Hoon a)
  | Tal (Hoon a)
  | Lus (Hoon a)
  | Tis (Hoon a) (Hoon a)
  --
  | The (Hoon a) (Hoon a)
  | Fas (Hoon a) (Hoon a)
  | Obj (Map Atom (Hoon a))
  | Cls (Map Atom (Hoon a))
  | Col Atom (Hoon a)
  --
  | Hol
  -- Runes
  | HaxBuc (Map Atom (Hoon a))
  | HaxCen (Map Atom (Hoon a))
  | HaxCol (Hoon a) (Scope () Hoon a)
  | HaxHep (Hoon a) (Scope () Hoon a)
  --
  | BarCen (Map Atom (Hoon a))
  | BarTis (Hoon a) (Scope () Hoon a)
  | CenDot (Hoon a) (Hoon a)
  | CenHep (Hoon a) (Hoon a)
  | ColHep (Hoon a) (Hoon a)
  | ColTar [Hoon a]
  | TisFas (Hoon a) (Scope () Hoon a)
  | DotDot (Hoon a) (Scope () Hoon a)
  | DotGal (Hoon a)
  | DotGar (Hoon a)
  | DotLus (Hoon a)
  | DotTis (Hoon a) (Hoon a)
  | KetFas (Hoon a) (Hoon a)
  | KetHep (Hoon a) (Hoon a)
  | WutCen (Hoon a) (Map Atom (Hoon a))
  | WutCol (Hoon a) (Hoon a) (Hoon a)
  deriving (Functor, Foldable, Traversable, Typeable)

deriveEq1   ''Hoon
deriveOrd1  ''Hoon
deriveRead1 ''Hoon
deriveShow1 ''Hoon

deriving instance Eq a   => Eq   (Hoon a)
deriving instance Ord a  => Ord  (Hoon a)
deriving instance Read a => Read (Hoon a)
deriving instance Show a => Show (Hoon a)

instance Applicative Hoon where
  pure = Var
  (<*>) = ap

instance Monad Hoon where
  return = Var
  --
  Var a >>= f = f a
  Met m >>= _ = Met m
  --
  Hax     >>= _ = Hax
  Fun t b >>= f = Fun (t >>= f) (b >>>= f)
  Cel t b >>= f = Cel (t >>= f) (b >>>= f)
  Wut ls  >>= _ = Wut ls
  Pat     >>= _ = Pat
  --
  Lam t b >>= f = Lam (t >>= f) (b >>>= f)
  Cns x y >>= f = Cns (x >>= f) (y >>= f) 
  Nat l   >>= _ = Nat l
  --
  App x y >>= f = App (x >>= f) (y >>= f)
  Hed x   >>= f = Hed (x >>= f)
  Tal x   >>= f = Tal (x >>= f)
  Lus x   >>= f = Lus (x >>= f)
  Tis x y >>= f = Tis (x >>= f) (y >>= f)
  --
  The x y >>= f = The (x >>= f) (y >>= f)
  Fas x y >>= f = Fas (x >>= f) (y >>= f)
  Obj cs  >>= f = Obj (cs  <&> (>>= f))
  Cls tcs >>= f = Cls (tcs <&> (>>= f))
  Col a x >>= f = Col a (x >>= f)
  --
  Hol >>= f = Hol
  --
  HaxBuc tcs >>= f = HaxBuc (tcs <&> (>>= f))
  HaxCen tcs >>= f = HaxCen (tcs <&> (>>= f))
  HaxCol x b >>= f = HaxCol (x >>= f) (b >>>= f)
  HaxHep x b >>= f = HaxHep (x >>= f) (b >>>= f)
  --
  BarCen cs    >>= f = BarCen (cs <&> (>>= f))
  BarTis x b   >>= f = BarTis (x >>= f) (b >>>= f)
  CenDot x y   >>= f = CenDot (x >>= f) (y >>= f)
  CenHep x y   >>= f = CenHep (x >>= f) (y >>= f)
  ColHep x y   >>= f = ColHep (x >>= f) (y >>= f)
  ColTar xs    >>= f = ColTar (xs <&> (>>= f))
  TisFas a b   >>= f = TisFas (a >>= f) (b >>>= f)
  DotDot t b   >>= f = DotDot (t >>= f) (b >>>= f)
  DotGal x     >>= f = DotGal (x >>= f)
  DotGar x     >>= f = DotGar (x >>= f)
  DotLus x     >>= f = DotLus (x >>= f)
  DotTis x y   >>= f = DotTis (x >>= f) (y >>= f)
  KetFas x y   >>= f = KetFas (x >>= f) (y >>= f)
  KetHep x y   >>= f = KetHep (x >>= f) (y >>= f)
  WutCen x cs  >>= f = WutCen (x >>= f) (cs <&> (>>= f))
  WutCol x y z >>= f = WutCol (x >>= f) (y >>= f) (z >>= f)
