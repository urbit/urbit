module DependentHoon3GoldenTests where

import ClassyPrelude

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Lazy as LT
import Data.Void
import System.FilePath (takeBaseName, replaceExtension)
import Test.Tasty
import Test.Tasty.Golden
import Text.Show.Pretty (ppShow)

import Practice.Render
import Practice.TopLevelDH3

listTests :: IO [FilePath]
listTests = findByExtension [".hoon"] "test/golden-dh3"

display :: (Show a, Rolling a) => a -> Text
display x = render x <> "\n\n" <> pack (ppShow x)

awful :: (Show a, Rolling a, Rolling b) => Either b a -> LBS.ByteString
awful = encodeUtf8 . either (LT.fromStrict . render) (LT.fromStrict . display)

renderLBS :: Rolling a => a -> LBS.ByteString
renderLBS = encodeUtf8 . LT.fromStrict . render

testEachPass :: FilePath -> TestTree
testEachPass file =
  goldenVsString baseName (replaceExtension file ".out") do
    txt <- readFileUtf8 file
    pure case road baseName scam txt of
      ResNone{err} ->
        "ERROR\n\n" <> renderLBS err
      ResRead{cst, ero} ->
        "PARSED\n\n" <> renderLBS cst <> "\n\nERROR\n\n" <> renderLBS ero
      ResOpen{cst, cod, ert} ->
        "PARSED\n\n" <> renderLBS cst <>
        "\n\nCODE\n\n" <> renderLBS cod <>
        "\n\nERROR\n\n" <> renderLBS ert
      ResType{cst, cod, cld, typ, bas} ->
        "PARSED\n\n" <> renderLBS cst <>
        "\n\nCODE\n\n" <> renderLBS cod <>
        "\n\nCOLD\n\n" <> renderLBS cld <>
        "\n\nTYPE\n\n" <> renderLBS typ <> "\n" <> encodeUtf8 (LT.fromStrict $ tshow typ) <>
        "\n\nBASE\n\n" <> renderLBS bas

 where
  baseName = takeBaseName file

testsIO :: IO TestTree
testsIO = do
  tests <- listTests
  pure $ testGroup "DependentLambda tests" $ fmap testEachPass tests


