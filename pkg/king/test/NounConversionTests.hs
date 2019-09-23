module NounConversionTests (tests) where

import Noun.Conversions
import UrbitPrelude

import Test.QuickCheck       hiding ((.&.))
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.TH

instance Arbitrary UV where
  arbitrary = UV <$> arbitrarySizedNatural

instance Arbitrary UW where
  arbitrary = UW <$> arbitrarySizedNatural


vRoundTrip :: UV -> Bool
vRoundTrip uv = Just uv == (fromNoun $ toNoun $ uv)

wRoundTrip :: UW -> Bool
wRoundTrip uw = Just uw == (fromNoun $ toNoun uw)


tests :: TestTree
tests =
  testGroup "Noun"
    [ testProperty "0v0 printing/parsing round trip" $ vRoundTrip
    , testProperty "0w0 printing/parsing round trip" $ wRoundTrip
    ]
