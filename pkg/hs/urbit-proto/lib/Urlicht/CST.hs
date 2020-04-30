module Urlicht.CST where

import ClassyPrelude
import Prelude (foldl1, foldr1)

import Bound
import Bound.Name
import Bound.Scope
import Control.Lens.Plated
import Control.Monad.State
import Data.Data (Data)
import Data.Data.Lens (uniplate)
import Numeric.Natural

import qualified Urlicht.Hoon as H
import Urlicht.Meta

type Atom = Natural

data CST
  = Var Text
  | Met Meta
  -- irregular forms
  | Hax
  | Fun [Binder] CST
  | Cel [Binder] CST
  | Wut (Set Atom)
  | Pat
  --
  | Lam [Binder] CST
  | Cns [CST]
  | Nat Atom
  --
  | App [CST]
  | Hed CST
  | Tal CST
  | Lus CST
  | Tis CST CST
  --
  | The CST CST
  | Fas CST CST
  | Obj (Map Atom CST)
  | Cls (Map Atom CST)
  | Col Atom CST
  --
  | Hol
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
  | DotGal CST
  | DotGar CST
  | DotLus CST
  | DotTis CST CST
  | KetFas CST CST
  | KetHep CST CST
  | WutCen CST (Map Atom CST)
  | WutCol CST CST CST
  deriving (Eq, Ord, Read, Show, Data, Typeable)

type Binder = (Maybe Text, CST)

instance Plated CST where
  plate = uniplate

abstractify :: CST -> H.Hoon Text
abstractify = go
  where
    go = \case
      Var v -> H.Var v
      Met m -> H.Met m
      --
      Hax -> H.Hax
      Fun bs c -> bindMany H.Fun bs (go c)
      Cel bs c -> bindMany H.Cel bs (go c)
      Wut s -> H.Wut s
      Pat -> H.Pat
      --
      Lam bs c -> bindMany H.Lam bs (go c)
      Cns cs -> foldr1 H.Cns $ go <$> cs
      Nat a -> H.Nat a
      --
      App cs -> foldl1 H.App $ go <$> cs
      Hed c -> H.Hed (go c)
      Tal c -> H.Tal (go c)
      Lus c -> H.Lus (go c)
      Tis c d -> H.Tis (go c) (go d)
      --
      The c d -> H.The (go c) (go d)
      Fas c d -> H.Fas (go c) (go d)
      Obj cs  -> H.Obj (go <$> cs)
      Cls tcs -> H.Cls (go <$> tcs)
      Col a c -> H.Col a (go c)
      --
      Hol -> H.Hol
      --
      HaxBuc tcs -> H.HaxBuc (go <$> tcs)
      HaxCen tcs -> H.HaxCen (go <$> tcs)
      HaxCol bs c -> bindMany H.HaxCol bs (go c)
      HaxHep bs c -> bindMany H.HaxHep bs (go c)
      --
      BarCen cs -> H.BarCen (go <$> cs)
      BarTis bs c -> bindMany H.BarTis bs (go c)
      CenDot c d -> H.CenDot (go c) (go d)
      CenHep c d -> H.CenHep (go c) (go d)
      ColHep c d -> H.ColHep (go c) (go d)
      ColTar cs -> H.ColTar (go <$> cs)
      TisFas v c d -> H.TisFas (go c) (abstract1 v $ go d)
      DotDot b c -> bind H.DotDot b (go c)
      DotGal c -> H.DotGal (go c)
      DotGar c -> H.DotGar (go c)
      DotLus c -> H.DotLus (go c)
      DotTis c d -> H.DotTis (go c) (go d)
      KetFas c d -> H.KetFas (go c) (go d)
      KetHep c d -> H.KetHep (go c) (go d)
      WutCen c cs -> H.WutCen (go c) (go <$> cs)
      WutCol c d e -> H.WutCol (go c) (go d) (go e)
    bind ctor (Just v,  c) h = ctor (go c) (abstract1 v h)
    bind ctor (Nothing, c) h = ctor (go c) (abstract (const Nothing) h)
    bindMany ctor bs h = foldr (bind ctor) h bs

concretize :: H.Hoon Text -> CST
concretize = dissociate . flip evalState 0 . go
  where
    go :: H.Hoon Text -> State Int CST
    go = \case
      H.Var v -> pure $ Var v
      H.Met m -> pure $ Met m
      --
      H.Hax -> pure $ Hax
      H.Fun t b -> unbindPoly Fun t b
      H.Cel t b -> unbindPoly Cel t b
      H.Wut s -> pure $ Wut s
      H.Pat -> pure $ Pat
      --
      H.Lam t b -> unbindPoly Lam t b
      H.Cns h j -> Cns <$> traverse go [h, j]
      H.Nat a -> pure $ Nat a
      --
      H.App h j -> App <$> traverse go [h, j]
      H.Hed c -> Hed <$> go c
      H.Tal c -> Tal <$> go c
      H.Lus c -> Lus <$> go c
      H.Tis c d -> Tis <$> go c <*> go d
      --
      H.The c d -> The <$> go c <*> go d
      H.Fas c d -> Fas <$> go c <*> go d
      H.Obj cs  -> Obj <$> traverse go cs
      --
      H.Cls tcs -> Cls <$> traverse go tcs
      H.Col a c -> Col a <$> go c
      --
      H.Hol -> pure $ Hol
      --
      H.HaxBuc tcs -> HaxBuc <$> traverse go tcs
      H.HaxCen tcs -> HaxCen <$> traverse go tcs

      H.HaxCol t b -> unbindPoly HaxCol t b
      H.HaxHep t b -> unbindPoly HaxHep t b

      H.BarCen cs -> BarCen <$> traverse go cs
      H.BarTis t b -> unbindPoly BarTis t b

      H.CenDot c d -> CenDot <$> go c <*> go d
      H.CenHep c d -> CenHep <$> go c <*> go d
      H.ColHep c d -> ColHep <$> go c <*> go d
      H.ColTar cs -> ColTar <$> traverse go cs
      H.TisFas h b -> do
        ((bnd, _), c) <- unbind H.Hax b
        h <- go h
        pure $ TisFas (fromMaybe "_" bnd) h c
      H.DotDot t b -> do
        (bnd, c) <- unbind t b
        pure $ DotDot bnd c
      H.DotGal c -> DotGal <$> go c
      H.DotGar c -> DotGar <$> go c
      H.DotLus c -> DotLus <$> go c
      H.DotTis c d -> DotTis <$> go c <*> go d
      H.KetFas c d -> KetFas <$> go c <*> go d
      H.KetHep c d -> KetHep <$> go c <*> go d
      H.WutCen c cs -> WutCen <$> go c <*> traverse go cs
      H.WutCol c d e -> WutCol <$> go c <*> go d <*> go e

    unbindPoly ctor t b = do
      (bdr, bod) <- unbind t b
      pure $ ctor [bdr] bod

    unbind :: H.Hoon Text -> Scope () H.Hoon Text -> State Int (Binder, CST)
    unbind t b = do
      -- TODO "_" and don't make a fresh var if not used
      v <- fresh
      t <- go t
      b <- go $ instantiate1 (H.Var v) b
      pure ((Just v, t), b)
    fresh :: State Int Text
    fresh = do
      i <- get
      modify (+ 1)
      pure $ "v" <> tshow i


    dissociate = transform \case
      Fun bs (Fun bs' c) -> Fun (bs <> bs') c
      Cel bs (Cel bs' c) -> Cel (bs <> bs') c
      Lam bs (Lam bs' c) -> Lam (bs <> bs') c
      Cns [c, Cns ds] -> Cns (c:ds)
      App (App cs : ds) -> App (cs <> ds)
      HaxCol bs (HaxCol bs' c) -> HaxCol (bs <> bs') c
      HaxHep bs (HaxHep bs' c) -> HaxHep (bs <> bs') c
      BarTis bs (BarTis bs' c) -> BarTis (bs <> bs') c
      c -> c
