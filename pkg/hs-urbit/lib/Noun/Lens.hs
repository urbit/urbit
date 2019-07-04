{-# LANGUAGE MagicHash #-}

module Noun.Lens where

import ClassyPrelude
import Noun.Pill
import Noun.Fat
import Noun
import Noun.Atom
import Control.Lens
import Noun.Jam.Fast (jam, jamBS, jamFat, jamFatBS)
import Noun.Cue.Fast (cue, cueBS, cueFat, cueFatBS)

--------------------------------------------------------------------------------

eitherToMaybe (Left _)  = Nothing
eitherToMaybe (Right x) = Just x

_CueFatBytes :: Prism' ByteString FatNoun
_CueFatBytes = prism' jamFatBS (eitherToMaybe . cueFatBS)

_CueFat :: Prism' Atom FatNoun
_CueFat = prism' jamFat (eitherToMaybe . cueFat)

_CueBytes :: Prism' ByteString Noun
_CueBytes = prism' jamBS (eitherToMaybe . cueBS)

_Cue :: Prism' Atom Noun
_Cue = prism' jam (eitherToMaybe . cue)

--------------------------------------------------------------------------------

loadNoun :: FilePath -> IO (Maybe FatNoun)
loadNoun = fmap (preview _CueFatBytes) . readFile

dumpJam :: FilePath -> FatNoun -> IO ()
dumpJam fp = writeFile fp . view (re _CueFatBytes)

tryCuePill :: PillFile -> IO ()
tryCuePill pill =
    loadNoun (show pill) >>= \case Nothing            -> print "nil"
                                   Just (FatAtom _ _) -> print "atom"
                                   Just (FatWord _)   -> print "word"
                                   _                  -> print "cell"

tryCueJamPill :: PillFile -> IO ()
tryCueJamPill pill = do
  n <- loadNoun (show pill) >>= \case
         Nothing                  -> print "failure" >> pure (FatWord 0)
         Just n@(FatAtom _ _)     -> print "atom"    >> pure n
         Just n@(FatCell _ _ _ _) -> print "cell"    >> pure n

  bs <- evaluate (force (jamFatBS n))

  print ("jam size: " <> show (length bs))
