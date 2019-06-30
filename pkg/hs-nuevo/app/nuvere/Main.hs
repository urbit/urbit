module Main where

import ClassyPrelude

import Types
import Nuevo
import Program

main :: IO ()
main = do
  let initEvent =
        NEvInit TopConnection (Path []) spawnsAnUnsafeMessagePrintingProgram (IoSocket 0 "base") "datum" :: NuevoEvent

  let (newState, effects) =
        (runNuevoFunction (emptyNuevoState, initEvent))

  print ("state: " ++ (show (nsProgramState newState)))
  print ("effects: " ++ (show effects))

  pure()
