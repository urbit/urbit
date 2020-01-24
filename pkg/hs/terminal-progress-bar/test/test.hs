{-# language OverloadedStrings #-}
{-# language PackageImports #-}

module Main where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import "base" System.Environment ( getArgs )
import "base" Data.Semigroup ( (<>) )
import "HUnit" Test.HUnit.Base ( assertEqual )
import "test-framework" Test.Framework
    ( defaultMainWithOpts, interpretArgsOrExit, Test, testGroup )
import "test-framework-hunit" Test.Framework.Providers.HUnit ( testCase )
import "terminal-progress-bar" System.ProgressBar
import qualified "text" Data.Text.Lazy as TL
import "time" Data.Time.Clock ( UTCTime(..), NominalDiffTime )

--------------------------------------------------------------------------------
-- Test suite
--------------------------------------------------------------------------------

main :: IO ()
main = do opts <- interpretArgsOrExit =<< getArgs
          defaultMainWithOpts tests opts

tests :: [Test]
tests =
  [ testGroup "Label padding"
    [ eqTest "no labels"  "[]"          mempty      mempty       0 $ Progress 0 0 ()
    , eqTest "pre"        "pre []"      (msg "pre") mempty       0 $ Progress 0 0 ()
    , eqTest "post"       "[] post"     mempty      (msg "post") 0 $ Progress 0 0 ()
    , eqTest "pre & post" "pre [] post" (msg "pre") (msg "post") 0 $ Progress 0 0 ()
    ]
  , testGroup "Bar fill"
    [ eqTest "empty"       "[....]" mempty mempty 6 $ Progress  0   1 ()
    , eqTest "almost half" "[=>..]" mempty mempty 6 $ Progress 49 100 ()
    , eqTest "half"        "[==>.]" mempty mempty 6 $ Progress  1   2 ()
    , eqTest "almost full" "[===>]" mempty mempty 6 $ Progress 99 100 ()
    , eqTest "full"        "[====]" mempty mempty 6 $ Progress  1   1 ()
    , eqTest "overfull"    "[====]" mempty mempty 6 $ Progress  2   1 ()
    ]
  , testGroup "Labels"
    [ testGroup "Percentage"
      [ eqTest "  0%" "  0% [....]" percentage mempty 11 $ Progress 0 1 ()
      , eqTest "100%" "100% [====]" percentage mempty 11 $ Progress 1 1 ()
      , eqTest " 50%" " 50% [==>.]" percentage mempty 11 $ Progress 1 2 ()
      , eqTest "200%" "200% [====]" percentage mempty 11 $ Progress 2 1 ()
      , labelTest "0 work todo" percentage (Progress 10 0 ()) "100%"
      ]
    , testGroup "Exact"
      [ eqTest "0/0" "0/0 [....]" exact mempty 10 $ Progress 0 0 ()
      , eqTest "1/1" "1/1 [====]" exact mempty 10 $ Progress 1 1 ()
      , eqTest "1/2" "1/2 [==>.]" exact mempty 10 $ Progress 1 2 ()
      , eqTest "2/1" "2/1 [====]" exact mempty 10 $ Progress 2 1 ()
      , labelTest "0 work todo" exact (Progress 10 0 ()) "10/0"
      ]
    , testGroup "Label Semigroup"
      [ eqTest "exact <> msg <> percentage"
               "1/2 -  50% [===>...]"
               (exact <> msg " - " <> percentage)
               mempty 20 $ Progress 1 2 ()
      ]
    , testGroup "rendeRuration"
      [ renderDurationTest 42 "42"
      , renderDurationTest (5 * 60 + 42) "05:42"
      , renderDurationTest (8 * 60 * 60 + 5 * 60 + 42) "08:05:42"
      , renderDurationTest (123 * 60 * 60 + 59 * 60 + 59) "123:59:59"
      ]
    ]
  ]

labelTest :: String -> Label () -> Progress () -> TL.Text -> Test
labelTest testName label progress expected =
    testCase testName $ assertEqual expectationError expected $ runLabel label progress someTiming

renderDurationTest :: NominalDiffTime -> TL.Text -> Test
renderDurationTest dt expected =
    testCase ("renderDuration " <> show dt) $ assertEqual expectationError expected $ renderDuration dt

eqTest :: String -> TL.Text -> Label () -> Label () -> Int -> Progress () -> Test
eqTest name expected mkPreLabel mkPostLabel width progress =
    testCase name $ assertEqual expectationError expected actual
  where
    actual = renderProgressBar style progress someTiming

    style :: Style ()
    style = defStyle
            { stylePrefix = mkPreLabel
            , stylePostfix = mkPostLabel
            , styleWidth = ConstantWidth width
            }

someTime :: UTCTime
someTime = UTCTime (toEnum 0) 0

someTiming :: Timing
someTiming = Timing someTime someTime

expectationError :: String
expectationError = "Expected result doesn't match actual result"
