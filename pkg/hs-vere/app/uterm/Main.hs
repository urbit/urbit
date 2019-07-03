module Main where

import ClassyPrelude
import Control.Lens
import Noun.Pill hiding (main)
import Noun.Lens

--------------------------------------------------------------------------------

main :: IO ()
main = do
  print "load brass" -- void getLine
  tryLoadPill Brass

  print "load ivory" -- void getLine
  tryLoadPill Ivory

  print "load solid" -- void getLine
  tryLoadPill Solid

  print "cue brass" -- void getLine
  tryCueJamPill Brass

  print "cue ivory" -- void getLine
  tryCueJamPill Ivory

  print "cue solid" -- void getLine
  tryCueJamPill Solid
