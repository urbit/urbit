module AmesTests (tests) where

import Data.Conduit
import Data.Conduit.List     hiding (take)
import Data.Ord.Unicode
import Test.QuickCheck       hiding ((.&.))
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.TH
import Urbit.Arvo
import Urbit.EventLog.LMDB
import Urbit.King.Config
import Urbit.Noun
import Urbit.Noun.Time
import Urbit.Prelude         hiding (elements)
import Urbit.Vere.Ames
import Urbit.Vere.Ames.Packet
import Urbit.Vere.Pier.Types
import Urbit.Vere.Ports

import Control.Concurrent (runInBoundThread)
import Data.Serialize     (decode, encode)
import Data.LargeWord     (LargeKey(..))
import GHC.Natural        (Natural)
import Network.Socket     (tupleToHostAddress)
import Urbit.King.App     (HasKingId(..))

import qualified Urbit.EventLog.LMDB as Log
import qualified Urbit.Noun.Time     as Time

packetSplitMorphism :: Packet -> Bool
packetSplitMorphism p = (decode . encode) p == Right p

tests :: TestTree
tests =
  testGroup "Ames"
    [ testProperty "Packet coding looks good" $
          packetSplitMorphism
    ]


-- Generate Arbitrary Values ---------------------------------------------------

arb :: Arbitrary a => Gen a
arb = arbitrary

instance Arbitrary Ipv4   where arbitrary = Ipv4 <$> arb
instance Arbitrary Port   where arbitrary = Port <$> arb
instance Arbitrary Wen    where arbitrary = Wen <$> arb
instance Arbitrary Gap    where arbitrary = Gap . abs <$> arb
instance Arbitrary Bytes  where arbitrary = pure (MkBytes "wtfbbq")
                                         -- MkBytes . take 100 <$> arb

instance Arbitrary a => Arbitrary (Patp a) where
    arbitrary = Patp <$> arb

instance Arbitrary ByteString where
  arbitrary = pack <$> arbitrary

instance Arbitrary Natural where
  arbitrary = fromIntegral . abs <$> (arbitrary :: Gen Integer)

instance (Arbitrary a, Arbitrary b) => Arbitrary (LargeKey a b) where
  arbitrary = LargeKey <$> arb <*> arb

genIpv4 :: Gen Ipv4
genIpv4 = do
  x <- arbitrary
  if (x == 0 || (x≥256 && x≤512))
    then genIpv4
    else pure (Ipv4 x)

instance Arbitrary Text where
  arbitrary = pack <$> arb

instance Arbitrary Cord where
  arbitrary = Cord <$> arb

instance Arbitrary BigCord where
  arbitrary = BigCord <$> arb

instance Arbitrary AmesDest where
  arbitrary = oneof [ EachYes <$> arb
                    , EachNo <$> arb
                    ]

instance Arbitrary a => Arbitrary (Jammed a) where
  arbitrary = Jammed <$> arbitrary

instance Arbitrary AmesAddress where
  arbitrary = AAIpv4 <$> arb <*> arb

instance Arbitrary Ship where
  arbitrary = Ship <$> elements
    [ 0
    , 42
    , 256
    , 24_530
    , 2_071_856_128
    , 2_824_325_100
    , 430_648_908_188_615_680
    , 2^60 + 1337
    ]

instance Arbitrary LogIdentity where
  arbitrary = LogIdentity <$> arb <*> arb <*> arb

instance Arbitrary Packet where
  arbitrary = do
    pktVersion   <- suchThat arb (< 8)
    pktSndr      <- arb
    pktRcvr      <- arb
    pktSndrTick  <- suchThat arb (< 16)
    pktRcvrTick  <- suchThat arb (< 16)
    pktOrigin    <- arb
    pktContent   <- arb
    pure Packet {..}
