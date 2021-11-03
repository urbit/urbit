module HoonSyntaxGoldenTests where

import ClassyPrelude

import Bound
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Lazy as LT
import Data.Void
import System.FilePath (takeBaseName, replaceExtension)
import Test.Tasty
import Test.Tasty.Golden
import Text.Show.Pretty (ppShow)
import Practice.HoonSyntax

listTests :: IO [FilePath]
listTests = findByExtension [".hoon"] "test/golden-syntax"

testEachPass :: FilePath -> TestTree
testEachPass file =
  let baseName = takeBaseName file
  in goldenVsString baseName (replaceExtension file ".cst") do
    txt <- readFileUtf8 file
    let cst = parse hoon baseName txt
    pure $ encodeUtf8 $ either LT.fromStrict (LT.pack . ppShow) cst

testsIO :: IO TestTree
testsIO = do
  tests <- listTests
  pure $ testGroup "HoonSyntax tests" $ fmap testEachPass tests


