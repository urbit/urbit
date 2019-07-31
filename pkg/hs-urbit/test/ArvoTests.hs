module ArvoTests (tests) where

import Data.Acquire
import Test.QuickCheck       hiding ((.&.))
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.TH
import UrbitPrelude
import Vere.Log
import Vere.Pier.Types
import Data.Conduit
import Data.Conduit.List
import Arvo
import Urbit.Time

import Control.Concurrent (threadDelay, runInBoundThread)
import Data.LargeWord     (LargeKey(..))
import GHC.Natural        (Natural)

import qualified Vere.Log as Log


-- Utils -----------------------------------------------------------------------

roundTrip :: forall a. (Eq a, ToNoun a, FromNoun a) => a -> Bool
roundTrip x = Just x == fromNoun (toNoun x)

nounEq :: (ToNoun a, ToNoun b) => a -> b -> Bool
nounEq x y = toNoun x == toNoun y

data EvExample = EvEx Ev Noun
  deriving (Eq, Show)

eventSanity :: [EvExample] -> Bool
eventSanity = all $ \(EvEx e n) -> toNoun e == n

instance Arbitrary EvExample where
  arbitrary = oneof $ fmap pure $
    [ EvEx (EvBlip $ BlipEvAmes $ AmesEvWant (Path []) (Ship 0) (Path []) (A 0))
           (toNoun (Path ["", "ames"], (Cord "want", (), (), ())))
    , EvEx (EvVane $ VaneVane $ VEVeer (Jael, ()) "" (Path []) "")
           (toNoun (Path ["vane", "vane", "jael"], Cord "veer", (), (), ()))
    ]

--------------------------------------------------------------------------------

tests :: TestTree
tests =
  testGroup "Arvo Events and Effects"
    [ testProperty "Round Trip Effect"  (roundTrip @Ef)
    , testProperty "Round Trip Event"   (roundTrip @Ev)
    , testProperty "Basic Event Sanity" eventSanity
    ]


-- Arbitrary Instances ---------------------------------------------------------

instance Arbitrary Natural where
  arbitrary = (fromIntegral . abs @Integer) <$> arb

newtype DumbChar = Dumb { unDumb :: Char }

instance Arbitrary DumbChar where
  arbitrary = Dumb <$> choose ('a', 'z')

instance Arbitrary Text where
  arbitrary = pack . fmap unDumb <$> arbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (LargeKey a b) where
  arbitrary = LargeKey <$> arb <*> arb

instance Arbitrary ByteString where
  arbitrary = encodeUtf8 <$> arbitrary

instance Arbitrary EvilPath where arbitrary = EvilPath <$> arb
instance Arbitrary Path     where arbitrary = Path <$> arb
instance Arbitrary Knot     where arbitrary = MkKnot <$> arb
instance Arbitrary Tape     where arbitrary = Tape <$> arb
instance Arbitrary BigTape  where arbitrary = BigTape <$> arb
instance Arbitrary Bytes    where arbitrary = MkBytes <$> arb
instance Arbitrary Octs     where arbitrary = Octs <$> arb
instance Arbitrary File     where arbitrary = File <$> arb
instance Arbitrary Cord     where arbitrary = Cord <$> arb
instance Arbitrary Wen      where arbitrary = Wen <$> arb
instance Arbitrary Gap      where arbitrary = Gap . abs <$> arb
instance Arbitrary Galaxy   where arbitrary = Galaxy <$> arb
instance Arbitrary Port     where arbitrary = Port <$> arb
instance Arbitrary Ship     where arbitrary = Ship <$> arb
instance Arbitrary Address  where arbitrary = AAmes <$> arb
instance Arbitrary Ipv4     where arbitrary = Ipv4 <$> arb

instance Arbitrary AmesDest where
  arbitrary = oneof [ ADGala <$> arb <*> arb
                    , ADIpv4 <$> arb <*> arb <*> arb
                    ]

instance Arbitrary Lane where
  arbitrary = If <$> arb <*> arb <*> arb

instance Arbitrary Ef where
  arbitrary = oneof [ EfVega <$> arb <*> arb
                    ]

instance Arbitrary AmesEv where
  arbitrary = oneof [ AmesEvHear () <$> arb     <*> arb
                    , AmesEvWake    <$> pure () <*> pure ()
                    , AmesEvWant    <$> arb     <*> arb     <*> arb <*> arb
                    ]

instance Arbitrary HttpRequest where
  arbitrary = HttpRequest <$> arb <*> arb <*> arb <*> arb

instance Arbitrary HttpServerReq where
  arbitrary = HttpServerReq <$> arb <*> arb <*> arb

instance Arbitrary HttpServerEv where
  arbitrary = oneof [ HttpServerEvRequest <$> arb <*> arb
                    , HttpServerEvLive    <$> arb <*> arb <*> arb
                    ]

instance Arbitrary BlipEv where
  arbitrary = oneof [ BlipEvAmes       <$> arb
                    , BlipEvHttpServer <$> arb
                    ]

instance Arbitrary Ev where
  arbitrary = oneof [ EvVane <$> arb
                    , EvBlip <$> arb
                    ]

instance Arbitrary Vane where
  arbitrary = oneof [ VaneVane <$> arb
                    , VaneZuse <$> arb
                    ]

instance Arbitrary VaneName where
  arbitrary = oneof $ pure <$> [minBound .. maxBound]

instance Arbitrary VaneEv where
  arbitrary = VEVeer <$> arb <*> arb <*> arb <*> arb

instance Arbitrary ZuseEv where
  arbitrary = ZEVeer () <$> arb <*> arb <*> arb


-- Generate Arbitrary Values ---------------------------------------------------

arb :: Arbitrary a => Gen a
arb = arbitrary
