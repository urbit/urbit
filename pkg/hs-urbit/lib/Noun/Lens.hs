{-# LANGUAGE MagicHash #-}

module Noun.Lens where

import ClassyPrelude
import Noun.Pill
import Noun
import Noun.Atom
import Control.Lens
import Noun.Jam.Fast (jam, jamBS)
import Noun.Cue.Fast (cue, cueBS)

--------------------------------------------------------------------------------

eitherToMaybe (Left _)  = Nothing
eitherToMaybe (Right x) = Just x

_CueBytes :: Prism' ByteString Noun
_CueBytes = prism' jamBS (eitherToMaybe . cueBS)

_Cue :: Prism' Atom Noun
_Cue = prism' jam (eitherToMaybe . cue)

loadNoun :: FilePath -> IO (Maybe Noun)
loadNoun = fmap (preview _CueBytes) . readFile

dumpJam :: FilePath -> Noun -> IO ()
dumpJam fp = writeFile fp . view (re _CueBytes)

tryCuePill :: PillFile -> IO ()
tryCuePill pill =
    loadNoun (show pill) >>= \case Nothing       -> print "nil"
                                   Just (Atom _) -> print "atom"
                                   _             -> print "cell"

tryCueJamPill :: PillFile -> IO ()
tryCueJamPill pill = do

  n <- loadNoun (show pill) >>= \case
         Nothing -> do print "failure"
                       pure (Atom 0)
         Just (Atom a) -> do print "atom"
                             pure (Atom a)
         Just (Cell h t) -> do print "cell"
                               pure (Cell h t)

  bs <- evaluate (force (jamBS n))

  print ("jam size: " <> show (length bs))
