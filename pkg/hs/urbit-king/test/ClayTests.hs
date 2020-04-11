module ClayTests (tests) where

import Urbit.Noun.Conversions
import Urbit.Prelude

import Test.QuickCheck       hiding ((.&.))
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.TH

instance Arbitrary Knot where
  arbitrary = (MkKnot . pack) <$> sublistOf ['a'..'z']

nonEmptyList :: (Arbitrary a) => Gen [a]
nonEmptyList = sized $ \n ->
  do k <- choose (1, max 1 n)
     vector k

instance Arbitrary Path where
  arbitrary = Path <$> nonEmptyList

testPathRoundTrip :: Path -> Property
testPathRoundTrip p =
  classify (1 == (length $ unPath p)) "singleton" $
  (filePathToPath (pathToFilePath p)) === p

tests = testGroup "Clay"
  [ testProperty "Path round trip" $ testPathRoundTrip ]
