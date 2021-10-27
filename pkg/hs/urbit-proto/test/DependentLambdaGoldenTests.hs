module DependentLambdaGoldenTests where

import ClassyPrelude

import qualified Data.ByteString.Lazy as LBS
import System.FilePath (takeBaseName, replaceExtension)
import Test.Tasty
import Test.Tasty.Golden

listTests :: IO [FilePath]
listTests = findByExtension [".hoon"] "test/golden-dl"

testEachPass :: FilePath -> TestTree
testEachPass file =
  let baseName = takeBaseName file
  in testGroup baseName
    [ goldenVsString (baseName <> " 0: id") (replaceExtension file ".0id") do
       LBS.readFile file
    ]

testsIO :: IO TestTree
testsIO = do
  tests <- listTests
  pure $ testGroup "DependentLambda tests" $ map testEachPass tests


