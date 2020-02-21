module Deppy.Hoon where

import ClassyPrelude
import Prelude (foldr1)

import Bound
import Bound.Name
import Bound.Scope
import Control.Monad.Morph (hoist)
import Control.Lens.Plated
import Data.Data (Data)
import Data.Data.Lens (uniplate)
import Data.Deriving (deriveEq1, deriveOrd1, deriveRead1, deriveShow1)
import Numeric.Natural

import qualified Deppy.Core as C

type Atom = Natural

data Hoon a
  = Var a
  -- irregular forms
  | Hax
  | Fun (Hoon a) (Scope (Name Text ()) Hoon a)
  | Cel (Hoon a) (Scope (Name Text ()) Hoon a)
  | Wut (Set Atom)
  | Pat
  --
  | Lam (Hoon a) (Scope (Name Text ()) Hoon a)
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
  -- Runes
  | HaxBuc (Map Atom (Hoon a))
  | HaxCen (Map Atom (Hoon a))
  | HaxCol (Hoon a) (Scope (Name Text ()) Hoon a)
  | HaxHep (Hoon a) (Scope (Name Text ()) Hoon a)
  --
  | BarCen (Map Atom (Hoon a))
  | BarTis (Hoon a) (Scope (Name Text ()) Hoon a)
  | CenDot (Hoon a) (Hoon a)
  | CenHep (Hoon a) (Hoon a)
  | ColHep (Hoon a) (Hoon a)
  | ColTar [Hoon a]
  | TisFas (Hoon a) (Scope (Name Text ()) Hoon a)
  | DotDot (Hoon a) (Scope (Name Text ()) Hoon a)
  | DotGal (Hoon a)
  | DotGar (Hoon a)
  | DotLus (Hoon a)
  | DotTis (Hoon a) (Hoon a)
  | KetFas (Hoon a) (Hoon a)
  | KetHep (Hoon a) (Hoon a)
  | WutCen (Hoon a) (Map Atom (Hoon a))
  | WutCol (Hoon a) (Hoon a) (Hoon a)
  | WutHax (Hoon a) (Map Atom (Scope (Name Text ()) Hoon a))
  deriving (Functor, Foldable, Traversable, Data, Typeable)

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
  WutHax x bs  >>= f = WutHax (x >>= f) (bs <&> (>>>= f))

instance (Data a) => Plated (Hoon a) where
  plate = uniplate

desugar :: Hoon a -> C.Exp a
desugar = go
  where
    go :: Hoon a -> C.Exp a
    go = \case
      Var v -> C.Var v
      --
      Hax     -> C.Typ
      Fun t b -> C.Fun $ C.Abs (go t) (hoist go b)
      Cel t b -> C.Cel $ C.Abs (go t) (hoist go b)
      Wut as  -> C.Atm (C.Wut as)
      Pat     -> C.Atm C.Pat
      --
      Lam t b -> C.Lam $ C.Abs (go t) (hoist go b)
      Cns h j -> C.Cns (go h) (go j) Nothing
      --
      Nat t   -> C.Nat t
      App h j -> C.App (go h) (go j)
      Hed h   -> C.Hed (go h)
      Tal h   -> C.Tal (go h)
      Lus h   -> C.Suc (go h)
      Tis h j -> C.Eqa (go h) (go j)
      --
      The ht c | cellular c -> C.Cns e f (Just $ go ht)
        where
          cellular = \case
            Cns{}    -> True
            ColHep{} -> True
            ColTar{} -> True
            _        -> False
          C.Cns e f _ = go c
      The ht hv -> the (go ht) (go hv)
      Fas hv ht -> go $ The ht hv
      Obj cs    -> go $ BarCen cs
      Cls tcs   -> go $ HaxCen tcs
      Col a h   -> go $ App h (Nat a)
      --
      HaxBuc tcs   -> C.Cel (mkCasAbs tcs)
      HaxCen tcs   -> C.Fun (mkCasAbs tcs)
      HaxCol t b   -> go $ Cel t b
      HaxHep t b   -> go $ Fun t b
      --
      BarCen cs    -> C.Lam (mkCasAbs cs)
      BarTis h b   -> C.Lam (C.Abs (go h) (hoist go b))
      CenDot h j   -> C.App (go j) (go h)
      CenHep h j   -> C.App (go h) (go j)
      ColHep h j   -> C.Cns (go h) (go j) Nothing
      ColTar hs    -> foldr1 (\e f -> C.Cns e f Nothing) $ go <$> hs
      TisFas h b   -> C.Let (go h) (hoist go b)
      DotDot h b   -> C.Rec $ C.Abs (go h) (hoist desugar b)
      DotGal h     -> C.Hed (go h)
      DotGar h     -> C.Tal (go h)
      DotLus h     -> C.Suc (go h)
      DotTis h j   -> C.Eqa (go h) (go j)
      KetFas hv ht -> go $ The ht hv
      KetHep ht hv -> go $ The ht hv
      WutCen h cs  -> C.Cas (go h) (go <$> cs)
      WutCol h j k -> C.Cas (go h) (mapFromList [(0, go j), (1, go k)])
      WutHax h bs  -> C.Mat (go h) (hoist go <$> bs)

free :: Applicative f => f a -> f (Var b (f a))
free = pure . F

mkCasAbs :: Map Atom (Hoon a) -> C.Abs a
mkCasAbs cs = C.Abs ty body
  where
    ty = C.Atm (C.Wut (keysSet cs))
    body = Scope $ C.Cas (C.Var $ B (Name "α" ())) (fmap (free . desugar) cs)

the :: C.Exp a -> C.Exp a -> C.Exp a
the t e = C.App (C.Lam $ C.Abs t (toScope $ C.Var $ B (Name "θ" ()))) e

resugar :: C.Exp a -> Hoon a
resugar = go
  where
    go :: C.Exp a -> Hoon a
    go = \case
      C.Var v -> Var v
      --
      C.Typ             -> Hax
      C.Fun (C.Abs t b) -> Fun (go t) (hoist go b)
      C.Cel (C.Abs t b) -> Cel (go t) (hoist go b)
      C.Atm (C.Wut as)  -> Wut as
      C.Atm C.Pat       -> Pat
      --
      C.Lam (C.Abs t b)  -> Lam (go t) (hoist go b)
      C.Cns e f (Just t) -> The (go t) (Cns (go e) (go f))
      C.Cns e f Nothing  -> Cns (go e) (go f)
      C.Nat a            -> Nat a
      --
      C.App e f  -> App (go e) (go f)
      C.Hed e    -> Hed (go e)
      C.Tal e    -> Tal (go e)
      C.Suc e    -> Lus (go e)
      C.Eqa e f  -> Tis (go e) (go f)
      C.Cas e cs -> WutCen (go e) (go <$> cs)
      C.Mat e bs -> WutHax (go e) (hoist go <$> bs)
      --
      C.Let e b         -> TisFas (go e) (hoist go b)
      C.Rec (C.Abs t b) -> DotDot (go t) (hoist go b)

-- FIXME it doesn't work inside scopes :/
resugar' :: Data a => C.Exp a -> Hoon a
resugar' = tr . resugar
  where
    tr :: (Data a) => Hoon a -> Hoon a
    -- this version diverges: tr = transform (change . dive). Why??
    tr = transform (change)
    change = \case
      Fun (Wut s) b@(Scope (WutCen (Var (B _)) cs))
        | [x] <- bindings b
        -> Cls $ (instantiate (const $ error "Do not want!") . Scope) <$> cs
      Cel (Wut s) b@(Scope (WutCen (Var (B _)) cs))
        | [x] <- bindings b
        -> HaxBuc $ (instantiate (const $ error "Do not want!") . Scope) <$> cs
      Lam (Wut s) b@(Scope (WutCen (Var (B _)) cs))
        | [x] <- bindings b
        -> Obj $ (instantiate (const $ error "Do not want!") . Scope) <$> cs
      -- TODO cas with 0 1 -> ?:
      h -> h
    -- too bad biplate/template seem broken
    dive :: (Data a) => Hoon a -> Hoon a
    dive = \case
      -- Make sure every ctor that has a Scope child appears here.
      Fun    h b -> Fun    h (hoistScope' tr b)
      Cel    h b -> Cel    h (hoistScope' tr b)
      Lam    h b -> Lam    h (hoistScope' tr b)
      HaxCol h b -> HaxCol h (hoistScope' tr b)
      HaxHep h b -> HaxHep h (hoistScope' tr b)
      BarTis h b -> BarTis h (hoistScope' tr b)
      TisFas h b -> TisFas h (hoistScope' tr b)
      DotDot h b -> DotDot h (hoistScope' tr b)
      h -> h

-- Same definition as Bound.Scope.hoistScope, but different type.
hoistScope' :: (Functor f, Data a, Data b, Data (g a))
            => (forall x. Data x => f x -> g x) -> Scope b f a -> Scope b g a
hoistScope' t (Scope b) = Scope $ t (fmap t <$> b)
