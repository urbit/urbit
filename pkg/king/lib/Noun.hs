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
    , LoadErr(..)
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

data LoadErr
    = FileErr IOException
    | CueErr DecodeErr
    | ParseErr [Text] Text
  deriving (Show)

instance Exception LoadErr

loadFile :: ∀a. FromNoun a => FilePath -> IO (Either LoadErr a)
loadFile pax = try $ do
    byt <- try (readFile pax) >>= either (throwIO . FileErr) pure
    non <- cueBS byt & either (throwIO . CueErr) pure
    res <- fromNounErr non & either (throwIO . uncurry ParseErr) pure
    pure res
