module Main (main) where

import ClassyPrelude

import Test.Tasty

-- import qualified DeppyCoreTests
import qualified DependentLambdaGoldenTests
import qualified DependentHoon3GoldenTests
import qualified DependentHoon4GoldenTests
import qualified DH3PropertyTests
import qualified HoonSyntaxGoldenTests

main :: IO ()
main = defaultMain =<< testGroup "Proto" <$> sequence
  [ DependentLambdaGoldenTests.testsIO
  , HoonSyntaxGoldenTests.testsIO
  -- , DependentHoon3GoldenTests.testsIO
  , DependentHoon4GoldenTests.testsIO
  , pure DH3PropertyTests.tests
  -- , pure DeppyCoreTests.tests
  ]
