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
import Vere.Drv.Ames
import Vere.Log
import Vere.Pier.Types

import Control.Concurrent (runInBoundThread)
import Data.LargeWord     (LargeKey(..))
import GHC.Natural        (Natural)
import KingApp            (App, PierEnv, runAppNoConfig, inPierEnvRAcquire)
import Network.Socket     (tupleToHostAddress)

import qualified Vere.Log as Log


-- Utils -----------------------------------------------------------------------

pid :: KingId
pid = KingId 0

turfEf :: NewtEf
turfEf = NewtEfTurf (0, ()) []

sendEf :: Galaxy -> Wen -> Bytes -> NewtEf
sendEf g w bs = NewtEfSend (0, ()) (ADGala w g) bs

{-
    Note that the Ames port is set to `Nothing` in the default config.
-}
runGala :: Word8 -> RAcquire App (TQueue Ev, EffCb NewtEf)
runGala point = do
    q <- newTQueueIO
    inPierEnvRAcquire who $ do
        let IODrv _ startDrv = ames pid True (writeTQueue q) print
        cb ← startDrv
        io (cb turfEf)
        pure (q, cb)
  where
    who :: Ship
    who = fromIntegral point

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

sendThread :: EffCb NewtEf -> (Galaxy, Bytes) -> RAcquire e ()
sendThread cb (to, val) =
    void (mkRAcquire start cancel)
  where
    start = async $ forever $ do threadDelay 1_000
                                 wen <- io $ now
                                 io $ cb $ sendEf to wen val
                                 threadDelay 10_000

zodSelfMsg :: Property
zodSelfMsg = forAll arbitrary (ioProperty . runAppNoConfig . runTest)
  where
    runTest :: Bytes -> RIO App Bool
    runTest val = runRAcquire $ do
      (zodQ, zod) <- runGala 0
      ()          <- sendThread zod (0, val)
      liftIO (waitForPacket zodQ val)

twoTalk :: Property
twoTalk = forAll arbitrary (ioProperty . runAppNoConfig . runTest)
  where
    runTest :: (Word8, Word8, Bytes) -> RIO App Bool
    runTest (aliceShip, bobShip, val) = do
      logInfo $ display $ tshow ("runTest", aliceShip, bobShip)
      if aliceShip == bobShip
        then pure True
        else go aliceShip bobShip val

    go :: Word8 -> Word8 -> Bytes -> RIO App Bool
    go aliceShip bobShip val = runRAcquire $ do
        logInfo $ display $ tshow ("runGala", aliceShip)
        (aliceQ, alice) <- runGala aliceShip
        logInfo $ display $ tshow ("runGala", bobShip)
        (bobQ,   bob)   <- runGala bobShip
        logInfo $ display $ tshow ("sendThread", aliceShip)
        sendThread alice (Galaxy bobShip,   val)
        logInfo $ display $ tshow ("sendThread", bobShip)
        sendThread bob  (Galaxy aliceShip, val)
        logInfo $ display $ tshow ("waitForPacket", aliceShip)
        liftIO (waitForPacket aliceQ val)
        logInfo $ display $ tshow ("waitForPacket", bobShip)
        liftIO (waitForPacket bobQ val)

tests :: TestTree
tests =
  testGroup "Ames"
    [ localOption (QuickCheckTests 10) $
          testProperty "Zod can send a message to itself" $
              zodSelfMsg

  -- XX Broken Test
  -- , localOption (QuickCheckTests 10) $
  --       testProperty "Two galaxies can talk" $
  --           twoTalk
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

genIpv4 ∷ Gen Ipv4
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
