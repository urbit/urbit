module Practice.RenderDH4Orphans where

import ClassyPrelude

import {-# SOURCE #-} Practice.DependentHoon4
import Practice.Render hiding (Line)

instance Rolling Soft

instance Var a => Rolling (Code a)

instance Var a => Rolling (Semi a)

instance Var a => Rolling (Con a)

instance Var a => Rolling (Line a)

instance Rolling Stub

instance Rolling Pelt

instance Rolling Fish

instance (Ord a, Rolling a) => Rolling (Set a)

instance Rolling [Fish]

instance Var a => Rolling (Cube a)

instance Rolling Warp

instance Var a => Rolling (Weft a)

instance Rolling Fail
