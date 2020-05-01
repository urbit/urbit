module Urlicht.SimpleToCoreHack where

-- |
-- This is a translation from Simple to Core which bypasses the elaborator,
-- erroring out if you use a construct which would require elaboration. The
-- point of it is to provide some syntax for core, so that we can more easily
-- test the machinery that manipulates cores without involving ourselves in
-- what is of course the most complex user of such machinery.

import ClassyPrelude

import Control.Monad.Morph (hoist)

import qualified Urlicht.Core as C
import Urlicht.Simple

down :: Simple a -> C.Core a
down = go
  where
    go :: Simple a -> C.Core a
    go = \case
      Var x -> C.Var x
      Met m -> C.Met m
      --
      Typ -> C.Typ
      Fun s ss -> C.Fun (go s) (hoist go ss)
      --
      Lam ss -> C.Lam (hoist go ss)
      --
      App s t -> C.App (go s) (go t)
      --
      Let s t ss -> C.Let (go s) (go t) (hoist go ss)
      --
      Hol -> error "SimpleToCoreHack.down: holes require elaboration"
      Asc s _ -> go s

-- This one is not a hack
up :: C.Core a -> Simple a
up = go
  where
    go :: C.Core a -> Simple a
    go = \case
      C.Var x -> Var x
      C.Met m -> Met m
      --
      C.Typ -> Typ
      C.Fun c sc -> Fun (go c) (hoist go sc)
      --
      C.Lam sc -> Lam (hoist go sc)
      --
      C.App c d -> App (go c) (go d)
      --
      C.Let c t sc -> Let (go c) (go t) (hoist go sc)
