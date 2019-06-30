module Main where

import ClassyPrelude

import Types
import Lib

main :: IO ()
main = do
  let initEvent =
        NEvInit TopConnection (Path []) unsafeMessagePrintingProgram (IoSocket 0 "base") "datum" :: NuevoEvent

  let (newState, _) =
        (runNuevoFunction (emptyNuevoState, initEvent))

  print (nsProgramState newState)

  pure()
