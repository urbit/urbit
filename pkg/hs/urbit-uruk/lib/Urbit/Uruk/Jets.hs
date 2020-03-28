module Urbit.Uruk.Jets where

import ClassyPrelude

import Urbit.Uruk.DashParser (DataJet(..), ExpTree(..), Pos, SingJet(..),
                              Ur(..), Val)

import qualified Urbit.Uruk.DashParser as Dash

--------------------------------------------------------------------------------

jetArity :: SingJet -> Pos
jetArity = $(pure Dash.jetArity)

jetTag :: SingJet -> Val
jetTag = $(pure Dash.jetTag)

jetBody :: SingJet -> Val
jetBody = $(pure Dash.jetBody)

jetMatch :: (Pos, Val, Val) -> Maybe SingJet
jetMatch = $(pure Dash.jetMatch)
