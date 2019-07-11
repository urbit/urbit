module Noun.Lens where

import ClassyPrelude
import Pill
import Noun
import Atom
import Control.Lens
import Jam (jam, jamBS)
import Cue (cue, cueBS)

--------------------------------------------------------------------------------

eitherToMaybe (Left _)  = Nothing
eitherToMaybe (Right x) = Just x

_CueBytes :: Prism' ByteString Noun
_CueBytes = prism' jamBS (eitherToMaybe . cueBS)

_Cue :: Prism' Atom Noun
_Cue = prism' jam (eitherToMaybe . cue)

--------------------------------------------------------------------------------

loadNoun :: FilePath -> IO (Maybe Noun)
loadNoun = fmap (preview _CueBytes) . readFile

dumpJam :: FilePath -> Noun -> IO ()
dumpJam fp = writeFile fp . view (re _CueBytes)

tryCuePill :: PillFile -> IO ()
tryCuePill pill =
    loadNoun (show pill) >>= \case Nothing         -> print "nil"
                                   Just (Atom _)   -> print "atom"
                                   Just (Cell _ _) -> print "cell"

tryCueJamPill :: PillFile -> IO ()
tryCueJamPill pill = do
  n <- loadNoun (show pill) >>= \case
         Nothing           -> print "failure" >> pure (Atom 0)
         Just n@(Atom _)   -> print "atom"    >> pure n
         Just n@(Cell _ _) -> print "cell"    >> pure n

  bs <- evaluate (force (jamBS n))

  print ("jam size: " <> show (length bs))
