module Deppy.Hoon where

import ClassyPrelude
import Prelude (foldr1)

import Bound
import Bound.Name
import Control.Monad.Morph (hoist)
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
  --
  | Lam (Hoon a) (Scope (Name Text ()) Hoon a)
  | Cns (Hoon a) (Hoon a)
  | Tag Atom
  --
  | App (Hoon a) (Hoon a)
  | Hed (Hoon a)
  | Tal (Hoon a)
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
  | KetFas (Hoon a) (Hoon a)
  | KetHep (Hoon a) (Hoon a)
  | WutCen (Hoon a) (Map Atom (Hoon a))
  deriving (Functor, Foldable, Traversable)

deriveEq1   ''Hoon
deriveOrd1  ''Hoon
deriveRead1 ''Hoon
makeBound   ''Hoon

deriving instance Eq a   => Eq   (Hoon a)
deriving instance Ord a  => Ord  (Hoon a)
deriving instance Read a => Read (Hoon a)

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
      Wut a   -> C.Wut a
      --
      Lam t b -> C.Lam $ C.Abs (go t) (hoist go b)
      Cns h j -> C.Cns (go h) (go j) Nothing
      --
      Tag t   -> C.Tag t
      App h j -> C.App (go h) (go j)
      Hed h   -> C.Hed (go h)
      Tal h   -> C.Tal (go h)
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
      Col a h   -> go $ App h (Tag a)
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
      KetFas hv ht -> go $ The ht hv
      KetHep ht hv -> go $ The ht hv
      WutCen h cs  -> C.Cas (go h) (go <$> cs)

free :: Applicative f => f a -> f (Var b (f a))
free = pure . F

mkCasAbs :: Map Atom (Hoon a) -> C.Abs a
mkCasAbs cs = C.Abs ty body
  where
    ty = C.Wut (keysSet cs)
    body = Scope $ C.Cas (C.Var $ B (Name "_" ())) (fmap (free . desugar) cs)

the :: C.Exp a -> C.Exp a -> C.Exp a
the t e = C.App (C.Lam $ C.Abs t (toScope $ C.Var $ B (Name "_" ()))) e

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
      C.Wut as          -> Wut as
      --
      C.Lam (C.Abs t b)  -> Lam (go t) (hoist go b)
      C.Cns e f (Just t) -> The (go t) (Cns (go e) (go f))
      C.Cns e f Nothing  -> Cns (go e) (go f)
      C.Tag a            -> Tag a
      --
      C.App e f  -> App (go e) (go f)
      C.Hed e    -> Hed (go e)
      C.Tal e    -> Tal (go e)
      C.Cas e cs -> WutCen (go e) (go <$> cs)
      --
      C.Let e b         -> TisFas (go e) (hoist go b)
      C.Rec (C.Abs t b) -> DotDot (go t) (hoist go b)
