module Urlicht.HoonToSimple where

import ClassyPrelude

import Bound
import Bound.Name
import Control.Monad.Morph (hoist)

import Deppy.Hoon
import qualified Urlicht.Simple as S

down :: Hoon a -> S.Simple a
down = undefined

up :: S.Simple a -> Hoon a
up = undefined
