module DependentLambdaGoldenTests where

import ClassyPrelude

import Bound
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Lazy as LT
import Data.Void
import System.FilePath (takeBaseName, replaceExtension)
import Test.Tasty
import Test.Tasty.Golden
import Text.Show.Pretty (ppShow)

import Practice.DependentLambda
import Practice.Hoon2DependentLambda
import Practice.HoonCommon
import Practice.HoonSyntax

listTests :: IO [FilePath]
listTests = findByExtension [".hoon"] "test/golden-dl"

testEachPass :: FilePath -> IO TestTree
testEachPass file = do
  let baseName = takeBaseName file
  txt <- readFileUtf8 file
  let cst = parse hoon baseName txt
  let cod = cst >>= open >>= maybe (Left "free variables") Right . closed
  let val = eval absurd <$> cod
  let typ = do c <- cod
               maybe (Left "type error") Right $
                 runReaderT (play (Con absurd absurd) c) []
  let o1  = encodeUtf8 $ LT.pack $ ppShow cst
  let o2  = encodeUtf8 $ LT.pack $ ppShow cod
  let o3a = encodeUtf8 $ LT.pack $ ppShow val
  let o3b = encodeUtf8 $ LT.pack $ ppShow typ
  pure $ testGroup baseName
    [ {-goldenVsString (baseName <> " 0: id") (replaceExtension file ".0id") do
        LBS.readFile file-}
      goldenVsString (baseName <> " 1: read") (replaceExtension file ".1cst") $
        pure o1
    , goldenVsString (baseName <> " 2: open") (replaceExtension file ".2cod") $
        pure o2
    , goldenVsString (baseName <> " 3a: eval") (replaceExtension file ".3val") $
        pure o3a
    , goldenVsString (baseName <> " 3b: type") (replaceExtension file ".3typ") $
        pure o3b
    ]

testsIO :: IO TestTree
testsIO = do
  tests <- listTests
  testGroup "DependentLambda tests" <$> traverse testEachPass tests


