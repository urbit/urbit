module Main (main) where

import ClassyPrelude

import Test.Tasty

-- import qualified DeppyCoreTests
import qualified DependentLambdaGoldenTests

main :: IO ()
main = defaultMain =<< testGroup "Proto" <$> sequence
  [ DependentLambdaGoldenTests.testsIO
  -- , pure DeppyCoreTests.tests
  ]
