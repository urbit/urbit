{-
   DONE Numberic jets are not matched, and therefore not evaluated in
        jet dashboard. What do?
   TODO Need to handle data jets too.
-}

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

jetTuple :: SingJet -> (Pos, Val, Val)
jetTuple sj = (jetArity sj, jetTag sj, jetBody sj)

jetUnMatch :: SingJet -> Val
jetUnMatch sj = jn ari :& tag :& bod
 where
  (ari, tag, bod) = jetTuple sj

jn :: Pos -> Val
jn 1 = N J
jn n = jn (n-1) :& N J
