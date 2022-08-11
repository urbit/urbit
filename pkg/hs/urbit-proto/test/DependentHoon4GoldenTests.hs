module DependentHoon4GoldenTests where

import ClassyPrelude

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Lazy as LT
import Data.Void
import System.FilePath (takeBaseName, replaceExtension)
import Test.Tasty
import Test.Tasty.Golden
import Text.Show.Pretty (ppShow)

import Practice.DependentHoon4 (loft, spin)
import Practice.Render
import Practice.TopLevelDH4
import Practice.Hoon2DependentHoon4 (shut')

listTests :: IO [FilePath]
listTests = findByExtension [".hoon"] "test/golden-dh4"

display :: (Show a, Rolling a) => a -> Text
display x = render x <> "\n\n" <> pack (ppShow x)

awful :: (Show a, Rolling a, Rolling b) => Either b a -> LBS.ByteString
awful = encodeUtf8 . either (LT.fromStrict . render) (LT.fromStrict . display)

renderLBS :: (Show a, Rolling a) => a -> LBS.ByteString
renderLBS x = encodeUtf8  $ LT.fromStrict $ render x
        --  <> "\n" <> pack (ppShow x)

testEachPass :: FilePath -> TestTree
testEachPass file =
  goldenVsString baseName (replaceExtension file ".out") do
    txt <- readFileUtf8 file
    case road baseName scam txt of
      ResNone{err} -> do
        pure $ "ERROR\n\n" <> renderLBS err
      ResRead{cst, ero} -> do
        pure $ "PARSED\n\n" <> renderLBS cst <> "\n\nERROR\n\n" <> renderLBS ero
      ResOpen{cst, sof, ert, mor} -> do
        writeFile (replaceExtension file ".trace") $ encodeUtf8 $ render mor
        pure $
          "PARSED\n\n" <> renderLBS cst <>
          "\n\nSOFT\n\n" <> renderLBS sof <>
          "\n\nHARD\n\n" <> renderLBS (shut' sof) <>
          "\n\nERROR\n\n" <> renderLBS ert
      ResType{cst, sof, cod, typ, ken, mor} -> do
        writeFile (replaceExtension file ".trace") $ encodeUtf8 $ render mor
        pure $
          "PARSED\n\n" <> renderLBS cst <>
          "\n\nSOFT\n\n" <> renderLBS sof <>
          "\n\nHARD\n\n" <> renderLBS (shut' sof) <>
          "\n\nCODE\n\n" <> renderLBS cod <>
          "\n\nTYPE\n\n" <> renderLBS (loft 0 typ) <>
              -- "\n\n" <> encodeUtf8 (LT.fromStrict $ tshow typ) <>
          "\n\nBASE\n\n" <> renderLBS (loft 0 ken)

 where
  baseName = takeBaseName file

testsIO :: IO TestTree
testsIO = do
  tests <- listTests
  pure $ localOption (mkTimeout 60_000_000)
       $ testGroup "DependentHoon4 golden tests"
       $ fmap testEachPass tests


