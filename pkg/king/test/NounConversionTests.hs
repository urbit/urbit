module NounConversionTests (tests) where

import Arvo.Event
import Noun.Conversions
import UrbitPrelude

import Crypto.Random.Types
import Test.QuickCheck        hiding ((.&.))
import Test.QuickCheck.Gen
import Test.QuickCheck.Random
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.TH

import qualified Crypto.ECC.Edwards25519 as Ed
import qualified Crypto.Error            as Ed
import qualified Data.ByteArray          as BA

-- String Representations of Atoms ---------------------------------------------

instance Arbitrary UV where
  arbitrary = UV <$> arbitrarySizedNatural

instance Arbitrary UW where
  arbitrary = UW <$> arbitrarySizedNatural

vRoundTrip :: UV -> Bool
vRoundTrip uv = Just uv == (fromNoun $ toNoun $ uv)

wRoundTrip :: UW -> Bool
wRoundTrip uw = Just uw == (fromNoun $ toNoun uw)

-- Cryptographic Point Representations -----------------------------------------

instance Crypto.Random.Types.MonadRandom Gen where
    getRandomBytes size = BA.pack <$> vector size

instance Arbitrary Ed.Point where
  arbitrary = Ed.toPoint <$> Ed.scalarGenerate

instance Arbitrary Ed.Scalar where
  arbitrary = Ed.scalarGenerate

passRoundTrip :: Ed.Point -> Ed.Point -> Bool
passRoundTrip crypt sign =
  Just val == (fromNoun $ toNoun val)
  where val = (Pass crypt sign)

ringRoundTrip :: Ed.Scalar -> Ed.Scalar -> Bool
ringRoundTrip crypt sign =
  Just val == (fromNoun $ toNoun val)
  where val = (Ring crypt sign)

--------------------------------------------------------------------------------

tests :: TestTree
tests =
  testGroup "Noun"
    [ testProperty "0v0 printing/parsing round trip" $ vRoundTrip
    , testProperty "0w0 printing/parsing round trip" $ wRoundTrip
    , testProperty "Pass round trip" $ passRoundTrip
    , testProperty "Ring round trip" $ ringRoundTrip
    ]
