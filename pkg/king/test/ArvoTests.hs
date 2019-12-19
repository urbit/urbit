module ArvoTests (tests) where

import Arvo
import Data.Acquire
import Data.Conduit
import Data.Conduit.List
import Data.Ord.Unicode
import Network.HTTP.Types.Method
import Test.QuickCheck           hiding ((.&.))
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.TH
import Urbit.Time
import UrbitPrelude
import Vere.Log
import Vere.Pier.Types

import Network.Socket     (tupleToHostAddress)
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
    [ EvEx (EvVane $ VaneVane $ VEVeer (Jael, ()) "" (Path []) "")
           (toNoun (Path ["vane", "vane", "jael"], Cord "veer", (), (), ()))
    ]

--------------------------------------------------------------------------------

tests :: TestTree
tests =
  testGroup "Arvo Events and Effects"
    [ testProperty "Round Trip Effect"   (roundTrip @Ef)
    , testProperty "Round Trip Event"    (roundTrip @Ev)
    , testProperty "Round Trip AmesDest" (roundTrip @AmesDest)
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
instance Arbitrary Port     where arbitrary = Port <$> arb
instance Arbitrary Ship     where arbitrary = Ship <$> arb
instance Arbitrary Address  where arbitrary = AAmes <$> arb

instance Arbitrary a => Arbitrary (Patp a) where
    arbitrary = Patp <$> arb

genIpv4 :: Gen Ipv4
genIpv4 = do
  x <- arbitrary
  if (x == 0 || (x≥256 && x≤512))
    then genIpv4
    else pure (Ipv4 x)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Each a b) where
  arbitrary = oneof [ EachNo <$> arb, EachYes <$> arb ]

instance (Arbitrary a) => Arbitrary (Jammed a) where
  arbitrary = Jammed <$> arbitrary

instance Arbitrary Ef where
  arbitrary = oneof [ EfVega <$> arb <*> arb
                    ]

instance Arbitrary AmesEv where
  arbitrary = oneof [ AmesEvHear () <$> arb <*> arb
                    , AmesEvHole () <$> arb <*> arb
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

instance Arbitrary StdMethod where
  arbitrary = oneof $ pure <$> [ minBound .. maxBound ]

instance Arbitrary Header where
  arbitrary = Header <$> arb <*> arb

instance Arbitrary BigCord where
  arbitrary = BigCord <$> arb


instance Arbitrary ServId where arbitrary = ServId <$> arb

instance Arbitrary UD where arbitrary = UD <$> arb
instance Arbitrary UV where arbitrary = UV <$> arb

instance Arbitrary AmesAddress where
  arbitrary = AAIpv4 <$> arb <*> arb

instance Arbitrary Ipv4   where arbitrary = Ipv4 <$> arb

-- Generate Arbitrary Values ---------------------------------------------------

arb :: Arbitrary a => Gen a
arb = arbitrary
