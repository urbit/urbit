module DependentLambdaGoldenTests where

import ClassyPrelude

import Bound
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Foldable as F
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
import Practice.Render

listTests :: IO [FilePath]
listTests = findByExtension [".hoon"] "test/golden-dl"

display :: (Show a, Rolling a) => a -> Text
display x = render x <> "\n\n" <> pack (ppShow x)

awful :: (Show a, Rolling a, Rolling b) => Either b a -> LBS.ByteString
awful = encodeUtf8 . either (LT.fromStrict . render) (LT.fromStrict . display)

data Err
  = ErrRead Text
  | ErrOpen Text
  | ErrFree [Term]
  | ErrType ([Act], Fail)

instance Rolling Err where
  roll = \case
    ErrRead t -> roll t
    ErrOpen t -> roll t
    ErrFree t -> leaf $ "free variables: " <> tshow t
    ErrType t -> roll t

testEachPass :: FilePath -> IO TestTree
testEachPass file = do
  let baseName = takeBaseName file
  txt <- readFileUtf8 file
  let cst = first ErrRead $ parse hoon baseName txt
  let cod = do
        c <- cst
        o <- first ErrOpen $ open c
        maybe (Left $ ErrFree $ F.toList o) Right $ closed o
  let val = eval absurd <$> cod
  let typ = do
        c <- cod
        first ErrType $ runReaderT (play (Con absurd absurd absurd) c) []
  let o1  = awful cst
  let o2  = awful cod
  let o3a = awful val
  let o3b = awful typ
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


