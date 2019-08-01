module AmesTests (tests) where

import Arvo
import Data.Acquire
import Data.Conduit
import Data.Conduit.List     hiding (take)
import Data.Ord.Unicode
import Noun
import Test.QuickCheck       hiding ((.&.))
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.TH
import Urbit.Time
import UrbitPrelude
import Vere.Ames
import Vere.Log
import Vere.Pier.Types

import Control.Concurrent (runInBoundThread, threadDelay)
import Data.LargeWord     (LargeKey(..))
import GHC.Natural        (Natural)
import Network.Socket     (tupleToHostAddress)

import qualified Vere.Log as Log


-- Utils -----------------------------------------------------------------------

pid :: KingInstance
pid = KingInst 0

turfEf :: NewtEf
turfEf = NewtEfTurf (0, ()) []

sendEf :: Galaxy -> Wen -> Bytes -> NewtEf
sendEf g w bs = NewtEfSend (0, ()) (ADGala w g) bs

runGala :: Word8 -> Acquire (TQueue Ev, EffCb NewtEf)
runGala point = do
    q  <- liftIO newTQueueIO
    cb <- snd $ ames pid (fromIntegral point) Nothing (writeTQueue q)
    liftIO $ cb turfEf
    pure (q, cb)

waitForPacket :: TQueue Ev -> Bytes -> IO Bool
waitForPacket q val = go
  where
    go =
      atomically (readTQueue q) >>= \case
        EvBlip (BlipEvAmes (AmesEvWake () ()))   -> go
        EvBlip (BlipEvAmes (AmesEvHear () _ bs)) -> pure (bs == val)
        _                                        -> pure False

runAcquire :: Acquire a -> IO a
runAcquire acq = with acq pure

sendThread :: EffCb NewtEf -> (Galaxy, Bytes) -> Acquire ()
sendThread cb (to, val) = void $ mkAcquire start cancel
  where
    start = async $ forever $ do threadDelay 1_000
                                 wen <- now
                                 cb (sendEf to wen val)
                                 threadDelay 10_000

zodSelfMsg :: Property
zodSelfMsg = forAll arbitrary (ioProperty . runTest)
  where
    runTest :: Bytes -> IO Bool
    runTest val = runAcquire $ do
      (zodQ, zod) <- runGala 0
      ()          <- sendThread zod (0, val)
      liftIO (waitForPacket zodQ val)

twoTalk :: Property
twoTalk = forAll arbitrary (ioProperty . runTest)
  where
    runTest :: (Word8, Word8, Bytes) -> IO Bool
    runTest (aliceShip, bobShip, val) =
      if aliceShip == bobShip
        then pure True
        else go aliceShip bobShip val

    go :: Word8 -> Word8 -> Bytes -> IO Bool
    go aliceShip bobShip val = runAcquire $ do
        (aliceQ, alice) <- runGala aliceShip
        (bobQ,   bob)   <- runGala bobShip
        sendThread alice (Galaxy bobShip,   val)
        sendThread bob   (Galaxy aliceShip, val)
        liftIO (waitForPacket aliceQ val >> waitForPacket bobQ val)

tests :: TestTree
tests =
  testGroup "Ames"
    [ localOption (QuickCheckTests 10) $
          testProperty "Zod can send a message to itself" $
              zodSelfMsg
    , localOption (QuickCheckTests 10) $
          testProperty "Two galaxies can talk" $
              twoTalk
    ]


-- Generate Arbitrary Values ---------------------------------------------------

arb :: Arbitrary a => Gen a
arb = arbitrary

instance Arbitrary Ipv4   where arbitrary = Ipv4 <$> arb
instance Arbitrary Port   where arbitrary = Port <$> arb
instance Arbitrary Wen    where arbitrary = Wen <$> arb
instance Arbitrary Gap    where arbitrary = Gap . abs <$> arb
instance Arbitrary Galaxy where arbitrary = Galaxy <$> arb
instance Arbitrary Bytes  where arbitrary = pure (MkBytes "wtfbbq")
                                         -- MkBytes . take 100 <$> arb

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

instance Arbitrary AmesDest where
  arbitrary = oneof [ ADGala <$> arb <*> arb
                    , ADIpv4 <$> arb <*> arb <*> genIpv4
                    ]

instance Arbitrary Ship where
  arbitrary = Ship <$> arb

instance Arbitrary LogIdentity where
  arbitrary = LogIdentity <$> arb <*> arb <*> arb
