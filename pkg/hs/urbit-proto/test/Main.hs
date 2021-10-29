module Main (main) where

import ClassyPrelude

import Test.Tasty

-- import qualified DeppyCoreTests
import qualified DependentLambdaGoldenTests
import qualified HoonSyntaxGoldenTests

main :: IO ()
main = defaultMain =<< testGroup "Proto" <$> sequence
  [ DependentLambdaGoldenTests.testsIO
  , HoonSyntaxGoldenTests.testsIO
  -- , pure DeppyCoreTests.tests
  ]
