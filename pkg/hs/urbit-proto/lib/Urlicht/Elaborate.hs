module Urlicht.Elaborate where

import ClassyPrelude

import Bound
import Control.Monad.Morph (hoist)

import Urlicht.Core
import qualified Urlicht.Simple as S

up :: (Meta -> a) -> Core a -> S.Simple a
up f = undefined {-go
  where
    go :: Core a -> S.Simple a
    go = \case
      Var x -> S.Var x
      Met m -> S.Var (f m)
      --
      Typ -> S.Typ
      Fun c sc -> S.Fun (go c) (hoist go sc)
      --
      Lam sc -> S.Lam (hoist go sc)
      --
      App c d -> S.App (go c) (go d)
      --
      Let c sc -> S.Let (go c) (hoist go sc)-}
