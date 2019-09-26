module HoonMapSetTests (tests) where

import UrbitPrelude

import Test.QuickCheck           hiding ((.&.))
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.TH


-- Utils -----------------------------------------------------------------------

roundTrip :: ∀a. Eq a => (a -> a) -> a -> Bool
roundTrip f x = f x == x

newtype SmallNoun = SN Noun
  deriving newtype (Eq, Ord, Show, ToNoun)

instance Arbitrary SmallNoun where
  arbitrary = SN <$> oneof [a, c, ac, ca, cc]
    where
      a  = A . fromIntegral <$> (arbitrary :: Gen Word8)
      c  = C <$> a <*> a
      ac = C <$> a <*> c
      ca = C <$> c <*> a
      cc = C <$> c <*> c

-- Props -----------------------------------------------------------------------

mapRoundtrip :: Map SmallNoun SmallNoun -> Bool
mapRoundtrip = roundTrip (mapFromHoonMap . mapToHoonMap)

setRoundtrip :: Set SmallNoun -> Bool
setRoundtrip = roundTrip (setFromHoonSet . setToHoonSet)


-- Utils -----------------------------------------------------------------------

tests :: TestTree
tests =
  testGroup "Map/Set Conversions"
    [ testProperty "Map Rountrip" mapRoundtrip
    , testProperty "Set Rountrip" setRoundtrip
    ]
