module Main where

import ClassyPrelude

import Vere
import Nuevo
import Program
import Types

main :: IO ()
main = do
  let initialState = initialVereEnv spawnsAnUnsafeMessagePrintingProgram

  ns <- vereStep initialState
  ns2 <- vereStep ns

  pure()
