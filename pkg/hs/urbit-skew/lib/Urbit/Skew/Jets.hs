{-
   DONE Numberic jets are not matched, and therefore not evaluated in
        jet dashboard. What do?
   DONE Need to handle data jets too.
-}

module Urbit.Skew.Jets
  ( sjArity, jetArity
  , sjTag, jetTag
  , sjBody, jetBody
  , sjTuple, jetTuple
  , sjMatch, jetMatch
  , sjUnMatch, jetUnMatch
  , module Urbit.Skew.Dash.DataJet
  , module Urbit.Skew.Dash.Exp
  )
where

import ClassyPrelude

import Data.List               (iterate, (!!))
import Numeric.Natural         (Natural)
import Urbit.Pos               (Pos)
import Urbit.Skew.Dash.DataJet (djArity, djBody, djMatch, djTag, djTuple, jn)
import Urbit.Skew.Dash.Exp     (DataJet(..), SingJet(..))
import Urbit.Skew.Dash.Exp     (ExpTree(..), Ur(..), Val)

import qualified Urbit.Atom             as Atom
import qualified Urbit.Skew.Dash.Parser as Dash


--------------------------------------------------------------------------------

sjArity :: SingJet -> Pos
sjArity = $(pure Dash.jetArity)

jetArity :: Either DataJet SingJet -> Pos
jetArity = either djArity sjArity


--------------------------------------------------------------------------------

sjTag :: SingJet -> Val
sjTag = $(pure Dash.jetTag)

jetTag :: Either DataJet SingJet -> Val
jetTag = either djTag sjTag


--------------------------------------------------------------------------------

sjBody :: SingJet -> Val
sjBody = $(pure Dash.jetBody)

jetBody :: Either DataJet SingJet -> Val
jetBody = either djBody sjBody


--------------------------------------------------------------------------------

sjTuple :: SingJet -> (Pos, Val, Val)
sjTuple sj = (sjArity sj, sjTag sj, sjBody sj)

jetTuple :: Either DataJet SingJet -> (Pos, Val, Val)
jetTuple = either djTuple sjTuple


--------------------------------------------------------------------------------

sjMatch :: (Pos, Val, Val) -> Maybe SingJet
sjMatch = $(pure Dash.jetMatch)

pattern SnTag = N (DataJet (NAT 28275))
pattern BnTag = N (DataJet (NAT 28258))
pattern CnTag = N (DataJet (NAT 28259))

jetMatch :: (Pos, Val, Val) -> Maybe (Either DataJet SingJet)
jetMatch tup = (Right <$> sjMatch tup) <|> (Left <$> djMatch tup)


--------------------------------------------------------------------------------

sjUnMatch :: SingJet -> Val
sjUnMatch sj = jn ari :& tag :& bod
 where
  (ari, tag, bod) = sjTuple sj

jetUnMatch :: Either DataJet SingJet -> Val
jetUnMatch jet = jn ari :& tag :& bod
 where
  (ari, tag, bod) = jetTuple jet
