module Deppy.Hoon where

import ClassyPrelude

import Bound
import Bound.Scope
import Control.Monad.Morph (hoist)
import Data.Deriving (deriveEq1, deriveOrd1, deriveRead1, deriveShow1)
import Data.Foldable (foldl1, foldr1)
import Data.List.NonEmpty (NonEmpty)
import Data.Word
import Numeric.Natural

import qualified Deppy.Core as C

type Atom = Natural

data Hoon a
  = Var a
  -- irregular forms
  | Hax
  | Fun (NonEmpty (Hoon a)) (Scope Word Hoon a)
  | Cel (NonEmpty (Hoon a)) (Scope Word Hoon a)
  | Wut (Set Atom)
  | Lam (NonEmpty (Hoon a)) (Scope Word Hoon a)
  | Cns (NonEmpty (Hoon a))
  | Tag Atom
  | App (NonEmpty (Hoon a))
  | Hed (Hoon a)
  | Tal (Hoon a)
  | The (Hoon a) (Hoon a)
  | Fas (Hoon a) (Hoon a)
  -- Runes
  | HaxBuc (Map Atom (Hoon a))
  | HaxCen (Map Atom (Hoon a))
  -- TODO bartis haxhep/haxtis haxcol/haxtar
  | BarCen (Map Atom (Hoon a))
  | CenDot (Hoon a) (Hoon a)
  | CenHep (Hoon a) (Hoon a)
  | TisFas (Hoon a) (Scope () Hoon a)
  | DotDot (Hoon a) (Scope () Hoon a)
  | KetFas (Hoon a) (Hoon a)
  | KetHep (Hoon a) (Hoon a)
  | WutCen (Hoon a) (Map Atom (Hoon a))
  deriving (Functor, Foldable, Traversable)

data PolyAbs a
  = Body (Hoon a)
  | Bind (Hoon a) (Scope () PolyAbs a)

deriveEq1   ''Hoon
deriveOrd1  ''Hoon
deriveRead1 ''Hoon
deriveShow1 ''Hoon
makeBound   ''Hoon

deriving instance Eq a   => Eq   (Hoon a)
deriving instance Ord a  => Ord  (Hoon a)
deriving instance Read a => Read (Hoon a)
deriving instance Show a => Show (Hoon a)

desugar :: Hoon a -> C.Exp a
desugar = go
  where
    go :: Hoon a -> C.Exp a
    go = \case
      Var v -> C.Var v
      Hax -> C.Typ
      --
      Wut a -> C.Wut a
      --
      Tag t -> C.Tag t
      App hs -> foldl1 C.App $ map go hs
      Hed h -> C.Hed (go h)
      Tal h -> C.Tal (go h)
      The ht c@Cns{} ->
        let C.Cns e f _ = go c
        in  C.Cns e f (Just $ go ht)
      The ht hv -> the (go ht) (go hv)
      Fas hv ht -> go $ The ht hv
      HaxBuc tcs -> C.Cel (mkCasAbs tcs)
      HaxCen tcs -> C.Fun (mkCasAbs tcs)
      BarCen cs -> C.Lam (mkCasAbs cs)
      CenDot h j -> C.App (go j) (go h)
      CenHep h j -> C.App (go h) (go j)
      TisFas h h' -> C.Let (go h) (hoist go h')
      DotDot h h' -> C.Rec $ C.Abs (go h) (hoist desugar h')
      -- TODO kethep etc on cells
      KetFas hv ht -> go $ The ht hv
      KetHep ht hv -> go $ The ht hv
      WutCen h cs -> C.Cas (go h) (go <$> cs)

free :: Applicative f => f a -> f (Var b (f a))
free = pure . F

mkCasAbs :: Map Atom (Hoon a) -> C.Abs a
mkCasAbs cs = C.Abs ty body
  where
    ty = C.Wut (keysSet cs)
    body = Scope $ C.Cas (C.Var $ B ()) (fmap (free . desugar) cs)

the :: C.Exp a -> C.Exp a -> C.Exp a
the t e = C.App (C.Lam $ C.Abs t (toScope $ C.Var $ B ())) e
