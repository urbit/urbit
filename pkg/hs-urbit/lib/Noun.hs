module Noun
    ( module Noun.Core
    , module Noun.Convert
    , module Noun.Conversions
    , module Noun.Atom
    , module Noun.Jam
    , module Noun.Cue
    , module Noun.TH
    , _Cue
    ) where

import ClassyPrelude
import Control.Lens

import Noun.Atom
import Noun.Conversions
import Noun.Convert
import Noun.Core
import Noun.Cue
import Noun.Jam
import Noun.TH

--------------------------------------------------------------------------------

_Cue :: Prism' ByteString Noun
_Cue = prism' jamBS (eitherToMaybe . cueBS)
  where
    eitherToMaybe (Left _)  = Nothing
    eitherToMaybe (Right x) = Just x
