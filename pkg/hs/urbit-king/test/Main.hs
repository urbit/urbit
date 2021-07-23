module Main (main) where

import ClassyPrelude

import Control.Concurrent    (runInBoundThread)
import Data.Proxy            (Proxy (Proxy))
import RIO.Directory
import Test.QuickCheck       hiding ((.&.))
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.TH
import Test.Tasty.Options    (OptionDescription (Option))

import qualified AmesTests
import qualified ArvoTests
import qualified BehnTests
import qualified ClayTests
import qualified DawnTests
import qualified DeriveNounTests
import qualified HoonMapSetTests
import qualified JamTests
import qualified LogTests
import qualified NounConversionTests
import qualified Options
import qualified Test.Tasty.Runners as Runners

main :: IO ()
main = do
  let ingredients =
        includingOptions
          [ Option (Proxy @Options.Brass)
          ] : defaultIngredients
    
  runInBoundThread $
    defaultMainWithIngredients ingredients $
      localOption (Runners.NumThreads 1) $
        testGroup "Urbit"
          [ AmesTests.tests
          , ArvoTests.tests
          , BehnTests.tests
          , ClayTests.tests
          , DawnTests.tests
          , DeriveNounTests.tests
          , HoonMapSetTests.tests
          , JamTests.tests
          , LogTests.tests
          , NounConversionTests.tests
          ]
