module Urbit.Urlicht.Meta (Meta, showMeta, readMeta) where

-- | A metavariable stands for a missing part of the program which the unifier
-- is to solve for. It's important to distinguish between metas and ordinary
-- variables, which are the things that you're (hopefully) already familiar
-- with from the lambda calculus. Unlike the latter, the former have no binding
-- structure.

import ClassyPrelude

import Data.List (genericIndex, elemIndex)
import Data.Maybe (fromJust)
import qualified Data.Text as T

type Meta = Int

showMeta :: Meta -> Text
showMeta = genericIndex names

-- FIXME
readMeta :: Text -> Meta
readMeta = fromJust . (flip elemIndex names)

names :: [Text]
names = fmap T.pack names'
  where
    names', letters :: [String]
    names' = letters ++ (do n <- names'; l <- letters; pure (n ++ l))
    letters = map (\x -> [x]) ['A'..'Z']
