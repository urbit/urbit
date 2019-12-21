module Deppy.CST where

import ClassyPrelude
import Prelude (foldl1, foldr1)

import Bound
import Bound.Name
import Numeric.Natural

import qualified Deppy.Hoon as H

type Atom = Natural

data CST
  = Var Text
  -- irregular forms
  | Hax
  | Fun [Binder] CST
  | Cel [Binder] CST
  | Wut (Set Atom)
  --
  | Lam [Binder] CST
  | Cns [CST]
  | Tag Atom
  --
  | App [CST]
  | Hed CST
  | Tal CST
  --
  | The CST CST
  | Fas CST CST
  | Obj (Map Atom CST)
  | Cls (Map Atom CST)
  | Col Atom CST
  -- Runes
  | HaxBuc (Map Atom CST)
  | HaxCen (Map Atom CST)
  | HaxCol [Binder] CST
  | HaxHep [Binder] CST
  --
  | BarCen (Map Atom CST)
  | BarTis [Binder] CST
  | CenDot CST CST
  | CenHep CST CST
  | ColHep CST CST
  | ColTar [CST]
  | TisFas Text CST CST
  | DotDot Binder CST
  | KetFas CST CST
  | KetHep CST CST
  | WutCen CST (Map Atom CST)
  deriving (Read, Eq, Ord)

type Binder = (Maybe Text, CST)

abstractify :: CST -> H.Hoon (Name Text Text)
abstractify = go
  where
    go = \case
      Var v -> H.Var (Name v v)
      --
      Hax -> H.Hax
      Wut s -> H.Wut s
      --
      Cns cs -> foldr1 H.Cns $ go <$> cs
      Tag a -> H.Tag a
      App cs -> foldl1 H.App $ go <$> cs
      Hed c -> H.Hed (go c)
      Tal c -> H.Tal (go c)
      --
      The c d -> H.The (go c) (go d)
      Fas c d -> H.Fas (go c) (go d)
      Obj cs  -> H.Obj (go <$> cs)
      Cls tcs -> H.Cls (go <$> tcs)
      Col a c -> H.Col a (go c)
      --
      HaxBuc tcs -> H.HaxBuc (go <$> tcs)
      HaxCen tcs -> H.HaxCen (go <$> tcs)
      --
      BarCen cs -> H.BarCen (go <$> cs)
    binder con bs c = foldr step c bs
      where
        step = undefined
    -- free = Scope . pure . F

concretize :: H.Hoon (Name Text a) -> CST
concretize = go
  where
    go = \case
      H.Var v -> Var (name v)

