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
import Practice.Render

listTests :: IO [FilePath]
listTests = findByExtension [".hoon"] "test/golden-syntax"

testEachPass :: FilePath -> IO [TestTree]
testEachPass file = do
  let baseName = takeBaseName file
  txt <- readFileUtf8 file
  let cst = parse vest baseName txt
  let prn = render <$> cst
  pure
    [ goldenVsString (baseName <> " cst") (replaceExtension file ".cst") $
        pure $ encodeUtf8 $ either LT.fromStrict (LT.pack . ppShow) cst
    , goldenVsString (baseName <> " pretty") (replaceExtension file ".prn") $
        pure $ encodeUtf8 $ either LT.fromStrict LT.fromStrict prn
    ]

testsIO :: IO TestTree
testsIO = do
  tests <- listTests
  testGroup "HoonSyntax tests" <$> concat <$> traverse testEachPass tests


