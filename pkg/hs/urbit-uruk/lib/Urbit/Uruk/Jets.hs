module Urbit.Uruk.Jets where

import ClassyPrelude

import Urbit.Uruk.DashParser (DataJet(..), ExpTree(..), Pos, SingJet(..),
                              Ur(..), Val)

import qualified Urbit.Uruk.DashParser as Dash

--------------------------------------------------------------------------------

jetArity :: SingJet -> Pos
jetArity = $(Dash.jetArity)

jetTag :: SingJet -> Val
jetTag = $(Dash.jetTag)

jetBody :: SingJet -> Val
jetBody = $(Dash.jetBody)

jetMatch :: (Pos, Val, Val) -> Maybe SingJet
jetMatch = $(Dash.jetMatch)
