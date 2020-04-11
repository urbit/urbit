module Urlicht.DisplayOrphans where

-- TODO make it not orphan when we move the higher-level infra from Deppy

import ClassyPrelude

import Deppy.Showings
import qualified Urlicht.Core as C
import qualified Urlicht.Elaborate as E
import qualified Urlicht.HoonToSimple as HS
import qualified Urlicht.Simple as S

instance Display (C.Value Text) where
  display = display . C.quote

instance Display (C.Core Text) where
  display = display . (E.up displayMeta)

instance Display (S.Simple Text) where
  display = display . HS.up

displayMeta :: C.Meta -> Text
displayMeta = (names !!)
  where
    names = letters ++ (names >>= (letters ++))
    letters = map singleton ['A'..'Z']
