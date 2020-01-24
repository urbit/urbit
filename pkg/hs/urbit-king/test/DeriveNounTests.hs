module DeriveNounTests (tests) where

import Data.Acquire
import Data.Conduit
import Data.Conduit.List
import Test.QuickCheck       hiding ((.&.))
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.TH
import Ur.Prelude
import Ur.Vere.Log
import Ur.Vere.Pier.Types

import Control.Concurrent (runInBoundThread, threadDelay)
import Data.LargeWord     (LargeKey(..))
import GHC.Natural        (Natural)

import qualified Ur.Vere.Log as Log


-- Sum Types -------------------------------------------------------------------

data Nums = One | Two | TwentyTwo | NineHundredNintyNine
  deriving (Eq, Show, Enum, Bounded)

data ThreeWords = ThreeWords Word Word Word
  deriving (Eq, Show)

data FooBar = FooBarQueenAlice Word Word
            | FooBarBob Word
            | FooBarCharlie
  deriving (Eq, Show)

data BarZaz = BZQueenAlice Word Word
            | BZBob Word
            | BZCharlie
  deriving (Eq, Show)

data ZazBaz = QueenAlice Word Word
            | Bob Word
            | Charlie
  deriving (Eq, Show)

data Empty

data Poly a b = PLeft a
              | PRite b
  deriving (Eq, Show)

deriveNoun ''Nums
deriveNoun ''ThreeWords
deriveNoun ''FooBar
deriveNoun ''BarZaz
deriveNoun ''ZazBaz
deriveNoun ''Empty
deriveNoun ''Poly

instance Arbitrary ThreeWords where
  arbitrary = ThreeWords <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Nums where
  arbitrary = oneof (pure <$> [ minBound .. maxBound ])

instance Arbitrary FooBar where
  arbitrary = oneof [ FooBarQueenAlice <$> arbitrary <*> arbitrary
                    , FooBarBob        <$> arbitrary
                    , pure FooBarCharlie
                    ]

instance Arbitrary BarZaz where
  arbitrary = oneof [ BZQueenAlice <$> arbitrary <*> arbitrary
                    , BZBob        <$> arbitrary
                    , pure BZCharlie
                    ]

instance Arbitrary ZazBaz where
  arbitrary = oneof [ QueenAlice <$> arbitrary <*> arbitrary
                    , Bob        <$> arbitrary
                    , pure Charlie
                    ]

instance (Arbitrary a, Arbitrary b) => Arbitrary (Poly a b) where
  arbitrary = oneof [ PLeft <$> arbitrary
                    , PRite <$> arbitrary
                    ]


-- Utils -----------------------------------------------------------------------

roundTrip :: forall a. (Eq a, ToNoun a, FromNoun a) => a -> Bool
roundTrip x = Just x == fromNoun (toNoun x)

throughNoun :: (ToNoun a, FromNoun b) => a -> Maybe b
throughNoun = fromNoun . toNoun

nounEquiv :: (Eq a, Eq b, ToNoun a, ToNoun b, FromNoun a, FromNoun b)
          => (a -> b) -> a -> Bool
nounEquiv cvt x =
    and [ Just x == throughNoun y
        , Just y == throughNoun x
        ]
  where y = cvt x


-- Sanity Checks ---------------------------------------------------------------

enumSanity :: Nums -> Bool
enumSanity x = toNoun x == byHand x
  where
    byHand = \case
      One                  -> toNoun (Cord "one")
      Two                  -> toNoun (Cord "two")
      TwentyTwo            -> toNoun (Cord "twenty-two")
      NineHundredNintyNine -> toNoun (Cord "nine-hundred-ninty-nine")

recSanity :: ThreeWords -> Bool
recSanity x = toNoun x == byHand x
  where
    byHand (ThreeWords x y z) = toNoun (x, y, z)

sumSanity :: ZazBaz -> Bool
sumSanity x = toNoun x == byHand x
  where
    byHand = \case
      QueenAlice x y       -> toNoun (Cord "queen-alice", x, y)
      Bob x                -> toNoun (Cord "bob", x)
      Charlie              -> toNoun (Cord "charlie")

abbrPrefixSanity :: BarZaz -> Bool
abbrPrefixSanity x = toNoun x == byHand x
  where
    byHand = \case
      BZQueenAlice x y -> toNoun (Cord "queen-alice", x, y)
      BZBob x          -> toNoun (Cord "bob", x)
      BZCharlie        -> toNoun (Cord "charlie")

typePrefixSanity :: FooBar -> Bool
typePrefixSanity x = toNoun x == byHand x
  where
    byHand = \case
      FooBarQueenAlice x y -> toNoun (Cord "queen-alice", x, y)
      FooBarBob x          -> toNoun (Cord "bob", x)
      FooBarCharlie        -> toNoun (Cord "charlie")


-- Strip Sum Prefixes ----------------------------------------------------------

barZazBaz :: BarZaz -> Bool
barZazBaz = nounEquiv $ \case BZQueenAlice x y -> QueenAlice x y
                              BZBob x          -> Bob x
                              BZCharlie        -> Charlie

fooBarBaz :: FooBar -> Bool
fooBarBaz = nounEquiv $ \case FooBarQueenAlice x y -> QueenAlice x y
                              FooBarBob x          -> Bob x
                              FooBarCharlie        -> Charlie


--------------------------------------------------------------------------------

tests :: TestTree
tests =
  testGroup "Log"
    [ testProperty "Enum Sanity"                 $ enumSanity
    , testProperty "Sum Sanity"                  $ sumSanity
    , testProperty "Record Sanity"               $ recSanity
    , testProperty "Type-Prefix Sanity"          $ abbrPrefixSanity
    , testProperty "Abbrv-Prefix Sanity"         $ typePrefixSanity
    , testProperty "Round Trip Rec (Poly)"       $ roundTrip @(Poly Bool Bool)
    , testProperty "Round Trip Rec (ThreeWords)" $ roundTrip @ThreeWords
    , testProperty "Round Trip Enum (Nums)"      $ roundTrip @Nums
    , testProperty "Round Trip Sum (FooBar)"     $ roundTrip @FooBar
    , testProperty "Round Trip Sum (BarZaz)"     $ roundTrip @BarZaz
    , testProperty "Round Trip Sum (ZazBaz)"     $ roundTrip @ZazBaz
    , testProperty "Prefix Test 1"               $ barZazBaz
    , testProperty "Prefix Test 2"               $ fooBarBaz
    ]


-- Generate Arbitrary Values ---------------------------------------------------

arb :: Arbitrary a => Gen a
arb = arbitrary
