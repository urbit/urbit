{-# LANGUAGE MagicHash #-}

module Data.Noun.Lens where

import ClassyPrelude
import Data.Noun.Pill
import Data.Noun
import Data.Noun.Atom
import Control.Lens
import Data.Noun.Jam.Put (jam, jamBS)
import Data.Noun.Jam (cue)

--------------------------------------------------------------------------------

_CueBytes :: Prism' ByteString Noun
_CueBytes = prism' jamBS unJamBS
  where unJamBS = preview (from pillBS . from pill . _Cue)

_Cue :: Prism' Atom Noun
_Cue = prism' jam cue

loadNoun :: FilePath -> IO (Maybe Noun)
loadNoun = fmap (preview $ from pillBS . from pill . _Cue) . readFile

dumpJam :: FilePath -> Noun -> IO ()
dumpJam fp = writeFile fp . view (re _Cue . pill . pillBS)

tryCuePill :: PillFile -> IO ()
tryCuePill pill =
    loadNoun (show pill) >>= \case Nothing       -> print "nil"
                                   Just (Atom _) -> print "atom"
                                   _             -> print "cell"
