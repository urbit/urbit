module Main where

import ClassyPrelude

import Nuevo
import Program
import Types
import Vere

main :: IO ()
main = do
  let initialState = initialVereEnv spawnsAnUnsafeMessagePrintingProgram

  ns <- vereStep initialState
  -- ns2 <- vereStep ns

  pure()
