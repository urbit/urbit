module Urlicht.SimpleToCoreHack where

-- |
-- This is a translation from Simple to Core which bypasses the elaborator,
-- erroring out if you use a construct which would require elaboration. The
-- point of it is to provide some syntax for core, so that we can more easily
-- tests the machinary that manipulates cores without involving ourselves in
-- what is of course the most complex user of such machinary.
--
-- The reverse translation is implemented in the display module, because the
-- way I've implemented it right now has it coupled with some printing logic
-- (in InjectMeta Text).

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
      --
      Typ -> C.Typ
      Fun s ss -> C.Fun (go s) (hoist go ss)
      --
      Lam ss -> C.Lam (hoist go ss)
      --
      App s t -> C.App (go s) (go t)
      --
      Let s ss -> C.Let (go s) (hoist go ss)
      --
      Hol -> error "SimpleToCoreHack.down: holes require elaboration"
