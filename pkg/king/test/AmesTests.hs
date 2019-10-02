module AmesTests (tests) where

import Arvo
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

import Control.Concurrent (runInBoundThread)
import Data.LargeWord     (LargeKey(..))
import GHC.Natural        (Natural)
import KingApp            (runApp)
import Network.Socket     (tupleToHostAddress)

import qualified Vere.Log as Log


-- Utils -----------------------------------------------------------------------

pid :: KingId
pid = KingId 0

turfEf :: NewtEf
turfEf = NewtEfTurf (0, ()) []

sendEf :: Galaxy -> Wen -> Bytes -> NewtEf
sendEf g w bs = NewtEfSend (0, ()) (ADGala w g) bs

runGala :: Word8 -> RAcquire e (TQueue Ev, EffCb e NewtEf)
runGala point = do
    q  <- newTQueueIO
    let (_, runAmes) = ames pid (fromIntegral point) Nothing (writeTQueue q)
    cb ← liftAcquire runAmes
    rio $ cb turfEf
    pure (q, cb)

waitForPacket :: TQueue Ev -> Bytes -> IO Bool
waitForPacket q val = go
  where
    go =
      atomically (readTQueue q) >>= \case
        EvBlip (BlipEvAmes (AmesEvWake () ()))   -> go
        EvBlip (BlipEvAmes (AmesEvHear () _ bs)) -> pure (bs == val)
        _                                        -> pure False

runRAcquire :: RAcquire e a -> RIO e a
runRAcquire acq = rwith acq pure

sendThread :: EffCb e NewtEf -> (Galaxy, Bytes) -> RAcquire e ()
sendThread cb (to, val) = void $ mkRAcquire start cancel
  where
    start = async $ forever $ do threadDelay 1_000
                                 wen <- io $ now
                                 cb (sendEf to wen val)
                                 threadDelay 10_000

zodSelfMsg :: Property
zodSelfMsg = forAll arbitrary (ioProperty . runApp . runTest)
  where
    runTest :: Bytes -> RIO e Bool
    runTest val = runRAcquire $ do
      (zodQ, zod) <- runGala 0
      ()          <- sendThread zod (0, val)
      liftIO (waitForPacket zodQ val)

twoTalk :: Property
twoTalk = forAll arbitrary (ioProperty . runApp . runTest)
  where
    runTest :: (Word8, Word8, Bytes) -> RIO e Bool
    runTest (aliceShip, bobShip, val) =
      if aliceShip == bobShip
        then pure True
        else go aliceShip bobShip val

    go :: Word8 -> Word8 -> Bytes -> RIO e Bool
    go aliceShip bobShip val = runRAcquire $ do
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

instance Arbitrary Text where
  arbitrary = pack <$> arb

instance Arbitrary Cord where
  arbitrary = Cord <$> arb

instance Arbitrary BigCord where
  arbitrary = BigCord <$> arb

instance Arbitrary AmesDest where
  arbitrary = oneof [ ADGala <$> arb <*> arb
                    , ADIpv4 <$> arb <*> arb <*> genIpv4
                    ]

instance Arbitrary Ship where
  arbitrary = Ship <$> arb

instance Arbitrary LogIdentity where
  arbitrary = LogIdentity <$> arb <*> arb <*> arb
