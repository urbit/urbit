module TryJamPill where

import ClassyPrelude
import Control.Lens
import Noun

--------------------------------------------------------------------------------

main :: IO ()
main = do
  print "cue brass" -- void getLine
  tryCueJamPill Brass

  print "cue ivory" -- void getLine
  tryCueJamPill Ivory

  print "cue solid" -- void getLine
  tryCueJamPill Solid

loadNoun :: FilePath -> IO (Maybe Noun)
loadNoun = fmap (preview _Cue) . readFile

dumpJam :: FilePath -> Noun -> IO ()
dumpJam fp = writeFile fp . view (re _Cue)

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

data PillFile = Brass | Ivory | Solid

instance Show PillFile where
  show = \case
    Brass -> "./bin/brass.pill"
    Solid -> "./bin/solid.pill"
    Ivory -> "./bin/ivory.pill"
