module AmesTests (tests) where

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
import Urbit.Ames
import Arvo
import Noun

import Control.Concurrent (threadDelay, runInBoundThread)
import Data.LargeWord     (LargeKey(..))
import GHC.Natural        (Natural)

import Urbit.Time
import qualified Vere.Log   as Log


-- Utils -----------------------------------------------------------------------

proc :: KingInstance
proc = KingInst 0

turfEf :: NewtEf
turfEf = NewtEfTurf (0, ()) []

sendEf :: Wen -> Bytes -> NewtEf
sendEf w bs = NewtEfSend (0, ()) (ADGala w 0) bs

zodSelfMsg :: Property
zodSelfMsg = forAll arbitrary (ioProperty . runTest)
  where
    runTest :: Natural -> IO Bool
    runTest val = do
      q <- newTQueueIO

      let (amesBorn, driver) =
            ames proc (Ship 0) Nothing (writeTQueue q)

      with driver $ \cb -> do

        cb turfEf

        let asdf = MkBytes "asdf"

        tSend <- async $ forever $ do
            threadDelay 1_000
            wen <- now
            cb (sendEf wen asdf)
            threadDelay 10_000

        let loop = do
              atomically (readTQueue q) >>= \case
                EvBlip (BlipEvAmes (AmesEvWake () ()))   -> loop
                EvBlip (BlipEvAmes (AmesEvHear () _ bs)) -> pure (bs == asdf)
                _                                        -> pure False
        res <- loop
        cancel tSend
        pure res


tests :: TestTree
tests =
  testGroup "Ames"
    [ localOption (QuickCheckTests 10) $
          testProperty "Zod can send a message to itself" $
              zodSelfMsg
    ]


-- Generate Arbitrary Values ---------------------------------------------------

arb :: Arbitrary a => Gen a
arb = arbitrary

instance Arbitrary Ipv4   where arbitrary = Ipv4 <$> arb
instance Arbitrary Port   where arbitrary = Port <$> arb
instance Arbitrary Wen    where arbitrary = Wen <$> arb
instance Arbitrary Gap    where arbitrary = Gap . abs <$> arb
instance Arbitrary Galaxy where arbitrary = Galaxy <$> arb

instance Arbitrary ByteString where
  arbitrary = pack <$> arbitrary

instance Arbitrary Natural where
  arbitrary = fromIntegral . abs <$> (arbitrary :: Gen Integer)

instance (Arbitrary a, Arbitrary b) => Arbitrary (LargeKey a b) where
  arbitrary = LargeKey <$> arb <*> arb

instance Arbitrary AmesDest where
  arbitrary = oneof [ ADGala <$> arb <*> arb
                    , ADIpv4 <$> arb <*> arb <*> arb
                    ]

instance Arbitrary Ship where
  arbitrary = Ship <$> arb

instance Arbitrary LogIdentity where
  arbitrary = LogIdentity <$> arb <*> arb <*> arb
