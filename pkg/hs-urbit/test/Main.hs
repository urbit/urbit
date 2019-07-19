module Main (main) where

import ClassyPrelude

import Test.QuickCheck       hiding ((.&.))
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.TH

import Control.Concurrent

import qualified LogTests

main :: IO ()
main =
  defaultMain $ testGroup "Urbit"
    [ LogTests.tests
    ]
