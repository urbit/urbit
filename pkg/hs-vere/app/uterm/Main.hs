module Main where

import ClassyPrelude
import Control.Lens
import Data.Noun.Pill hiding (main)
import Data.Noun.Lens

--------------------------------------------------------------------------------

main :: IO ()
main = do
  print "load brass" >> void getLine
  tryLoadPill Brass

  print "load ivory" >> void getLine
  tryLoadPill Ivory

  print "load solid" >> void getLine
  tryLoadPill Solid

  print "cue brass" >> void getLine
  tryCuePill Brass

  print "cue ivory" >> void getLine
  tryCuePill Ivory

  print "cue solid" >> void getLine
  tryCuePill Solid
