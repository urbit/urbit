module Main (main) where

import ClassyPrelude

import RIO.Directory
import Test.QuickCheck       hiding ((.&.))
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.TH

import Control.Concurrent (runInBoundThread)
import System.Environment (setEnv)

import qualified AmesTests
import qualified ArvoTests
import qualified BehnTests
import qualified DawnTests
import qualified DeriveNounTests
import qualified HoonMapSetTests
import qualified LogTests
import qualified NounConversionTests

main :: IO ()
main = do
    makeAbsolute "../.." >>= setCurrentDirectory
    setEnv "TASTY_NUM_THREADS" "1"
    runInBoundThread $ defaultMain $ testGroup "Urbit"
        [ AmesTests.tests
        , ArvoTests.tests
        , BehnTests.tests
        , DawnTests.tests
        , DeriveNounTests.tests
        , HoonMapSetTests.tests
        , LogTests.tests
        , NounConversionTests.tests
        ]
