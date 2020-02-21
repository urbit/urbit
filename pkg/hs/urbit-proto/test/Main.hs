module Main (main) where

import ClassyPrelude

import Test.Tasty

import qualified DeppyCoreTests

main :: IO ()
main = defaultMain $ testGroup "Proto"
  [ DeppyCoreTests.tests
  ]
