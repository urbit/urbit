module Main (main) where

import ClassyPrelude

import Test.Tasty

-- import qualified DeppyCoreTests
import qualified DependentLambdaGoldenTests
import qualified DependentHoon3GoldenTests
import qualified HoonSyntaxGoldenTests

main :: IO ()
main = defaultMain =<< testGroup "Proto" <$> sequence
  [ DependentLambdaGoldenTests.testsIO
  , HoonSyntaxGoldenTests.testsIO
  , DependentHoon3GoldenTests.testsIO
  -- , pure DeppyCoreTests.tests
  ]
