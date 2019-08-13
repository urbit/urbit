module Noun
    ( module Noun.Atom
    , module Data.Word
    , module Noun.Conversions
    , module Noun.Convert
    , module Noun.Core
    , module Noun.Cue
    , module Noun.Jam
    , module Noun.Tank
    , module Noun.TH
    , _Cue
    , loadFile
    ) where

import ClassyPrelude
import Control.Lens

import Data.Word
import Noun.Atom
import Noun.Conversions
import Noun.Convert
import Noun.Core
import Noun.Cue
import Noun.Jam
import Noun.Tank
import Noun.TH

--------------------------------------------------------------------------------

_Cue :: Prism' ByteString Noun
_Cue = prism' jamBS (eitherToMaybe . cueBS)
  where
    eitherToMaybe (Left _)  = Nothing
    eitherToMaybe (Right x) = Just x

data LoadErr = CueErr DecodeErr
             | ParseErr [Text] Text
  deriving (Eq, Ord, Show)

loadFile :: ∀a. FromNoun a => FilePath -> IO (Either LoadErr a)
loadFile pax = do
    bs <- readFile pax
    case cueBS bs of
      Left e  -> pure $ Left (CueErr e)
      Right n -> case fromNounErr n of
                   Left (p,e) -> pure $ Left (ParseErr p e)
                   Right x    -> pure $ Right x
