{-|
    Noun Library

    This module just re-exports things from submodules.
-}
module Urbit.Noun
    ( module Urbit.Atom
    , module Data.Word
    , module Urbit.Noun.Conversions
    , module Urbit.Noun.Convert
    , module Urbit.Noun.Core
    , module Urbit.Noun.Cue
    , module Urbit.Noun.Jam
    , module Urbit.Noun.Tank
    , module Urbit.Noun.TH
    , module Urbit.Noun.Tree
    , _Cue
    , LoadErr(..)
    , loadFile
    ) where

import ClassyPrelude
import Control.Lens

import Data.Word
import Urbit.Atom
import Urbit.Noun.Conversions
import Urbit.Noun.Convert
import Urbit.Noun.Core
import Urbit.Noun.Cue
import Urbit.Noun.Jam
import Urbit.Noun.Tank
import Urbit.Noun.TH
import Urbit.Noun.Tree

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
