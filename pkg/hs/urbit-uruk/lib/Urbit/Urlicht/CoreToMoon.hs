module Urbit.Urlicht.CoreToMoon where

import ClassyPrelude

import Bound
import Control.Monad.Morph (hoist)

import Urbit.Urlicht.Core
import Urbit.Urlicht.Meta
import qualified Urbit.Moon.AST as M

-- TODO erasure lol
down :: Core a -> M.Exp a
down = go where
  go :: Core a -> M.Exp a
  go = \case
    Var v -> M.Var v
    Met m -> terror ("CoreToMoon: unsolved meta " <> showMeta m)
    --
    Typ -> M.Str "type"
    Fun{} -> M.Str "function-type"
    --
    Lam sc -> M.Lam (hoist go sc)
    --
    App c d -> M.App (go c) (go d)
    --
    Let _ rhs bod -> M.Let (go rhs) (hoist go bod)
