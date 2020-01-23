module Ur.Noun
    ( module Ur.Noun.Atom
    , module Data.Word
    , module Ur.Noun.Conversions
    , module Ur.Noun.Convert
    , module Ur.Noun.Core
    , module Ur.Noun.Cue
    , module Ur.Noun.Jam
    , module Ur.Noun.Tank
    , module Ur.Noun.TH
    , module Ur.Noun.Tree
    , _Cue
    , LoadErr(..)
    , loadFile
    ) where

import ClassyPrelude
import Control.Lens

import Data.Word
import Ur.Noun.Atom
import Ur.Noun.Tree
import Ur.Noun.Conversions
import Ur.Noun.Convert
import Ur.Noun.Core
import Ur.Noun.Cue
import Ur.Noun.Jam
import Ur.Noun.Tank
import Ur.Noun.TH

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
