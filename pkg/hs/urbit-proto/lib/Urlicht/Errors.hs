module Urlicht.Errors where

import ClassyPrelude

data Error
  = UnknownError

instance (Show Error) where
  show = \case
    UnknownError -> "Very sorry sir, but your program does not compile."
