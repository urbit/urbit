module Urlicht.HoonToSimple where

import ClassyPrelude

import Control.Monad.Morph (hoist)

-- TODO copy over the upper layers of deppy to separately customize.
import Urlicht.Hoon
import qualified Urlicht.Simple as S

down :: Hoon a -> S.Simple a
down = go
  where
    go :: Hoon a -> S.Simple a
    go = \case
      Var v -> S.Var v
      Met m -> S.Met m
      --
      Hax     -> S.Typ
      Fun t b -> S.Fun (go t) (hoist go b)
      Cel _ _ -> error "down: unsupported feature: [||]"
      Wut _   -> error "down: unsupported feature: ?()"
      Pat     -> error "down: unsupported feature: @"
      --
      Lam Hol b -> S.Lam (hoist go b)
      Lam _ b -> error "down: no types on lambda args. trust the process."
      Cns _ _ -> error "down: unsupported feature: []"
      --
      Nat _   -> error "down: unsupported feature: 0"
      App h j -> S.App (go h) (go j)
      Hed h   -> error "down: unsupported feature: -."
      Tal h   -> error "down: unsupported feature: +."
      Lus h   -> error "down: unsupported feature: +()"
      Tis h j -> error "down: unsupported feature: =()"
      --
      {-The ht c | cellular c -> C.Cns e f (Just $ go ht)
        where
          cellular = \case
            Cns{}    -> True
            ColHep{} -> True
            ColTar{} -> True
            _        -> False
          C.Cns e f _ = go c-}
      The ht hv -> S.Asc (go hv) (go ht)
      Fas hv ht -> S.Asc (go hv) (go ht)
      Obj cs    -> error "down: unsupported feature: {}"
      Cls tcs   -> error "down: unsupported feature: {||}"
      Col a h   -> error "down: unsupported feature: x:"
      --
      Hol -> S.Hol
      --
      HaxBuc tcs   -> error "down: unsupported feature: #$"
      HaxCen tcs   -> error "down: unsupported feature: #%"
      HaxCol t b   -> go $ Cel t b
      HaxHep t b   -> go $ Fun t b
      --
      BarCen cs    -> error "down: unsupported feature: |%"
      BarTis Hol b -> S.Lam (hoist go b)
      BarTis _ b   -> error "down: no types on lambda args. trust the process."
      CenDot h j   -> S.App (go j) (go h)
      CenHep h j   -> S.App (go h) (go j)
      ColHep h j   -> error "down: unsupported feature: :-"
      ColTar hs    -> error "down: unsupported feature: :*"
      TisFas h t b -> S.Let (go h) (go t) (hoist go b)
      DotDot h b   -> error "down: unsupported feature: .."
      DotGal h     -> error "down: unsupported feature: .<"
      DotGar h     -> error "down: unsupported feature: .>"
      DotLus h     -> error "down: unsupported feature: .+"
      DotTis h j   -> error "down: unsupported feature: .="
      KetFas hv ht -> go $ The ht hv
      KetHep ht hv -> go $ The ht hv
      WutCen h cs  -> error "down: unsupported feature: ?%"
      WutCol h j k -> error "down: unsupported feature: ?:"


up :: S.Simple a -> Hoon a
up = go
  where
    go :: S.Simple a -> Hoon a
    go = \case
      S.Var x -> Var x
      S.Met m -> Met m
      --
      S.Typ -> Hax
      S.Fun s ss -> Fun (go s) (hoist go ss)
      --
      S.Lam ss -> Lam Hol (hoist go ss)
      --
      S.App s t -> App (go s) (go t)
      --
      S.Let s t ss -> TisFas (go s) (go t) (hoist go ss)
      --
      S.Hol -> Hol
      S.Asc s t -> The (go t) (go s)
