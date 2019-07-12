module Lib where

import ClassyPrelude

import Types

someFunc :: IO ()
someFunc = putStrLn "someFunc"






runNuevoFunction :: NuevoFunction
runNuevoFunction (NuevoState{..}, NEvInit i) = (NuevoState{..}, [])
