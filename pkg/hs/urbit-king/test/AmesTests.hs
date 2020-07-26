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
import Urbit.Prelude
import Urbit.Vere.Ames
import Urbit.Vere.Pier.Types

import Control.Concurrent (runInBoundThread)
import Data.LargeWord     (LargeKey(..))
import GHC.Natural        (Natural)
import Network.Socket     (tupleToHostAddress)
import Urbit.King.App     (HasKingId(..))

import qualified Urbit.EventLog.LMDB as Log


--------------------------------------------------------------------------------

type HasAmes e = (HasLogFunc e, HasNetworkConfig e, HasKingId e)

-- Utils -----------------------------------------------------------------------

pid :: KingId
pid = KingId 0

turfEf :: NewtEf
turfEf = NewtEfTurf (0, ()) []

sendEf :: Galaxy -> Wen -> Bytes -> NewtEf
sendEf g w bs = NewtEfSend (0, ()) (EachYes g) bs

data NetworkTestApp = NetworkTestApp
    { _ntaLogFunc       :: !LogFunc
    , _ntaNetworkConfig :: !NetworkConfig
    , _ntaKingId        :: !Word16
    }

makeLenses ''NetworkTestApp

instance HasLogFunc NetworkTestApp where
  logFuncL = ntaLogFunc

instance HasNetworkConfig NetworkTestApp where
  networkConfigL = ntaNetworkConfig

instance HasKingId NetworkTestApp where
  kingIdL = ntaKingId

runNetworkApp :: RIO NetworkTestApp a -> IO a
runNetworkApp = runRIO NetworkTestApp
  { _ntaLogFunc       = mkLogFunc (\_ _ _ _ -> pure ())
  , _ntaKingId        = 34
  , _ntaNetworkConfig = NetworkConfig { _ncNetMode   = NMNormal
                                      , _ncAmesPort  = Nothing
                                      , _ncNoAmes    = False
                                      , _ncNoHttp    = False
                                      , _ncNoHttps   = False
                                      , _ncHttpPort  = Nothing
                                      , _ncHttpsPort = Nothing
                                      , _ncLocalPort = Nothing
                                      }
  }

runGala
  :: forall e
   . HasAmes e
  => Word8
  -> RAcquire e (TQueue EvErr, NewtEf -> IO ())
runGala point = do
    env <- ask
    que <- newTQueueIO
    let enqueue = \p -> writeTQueue que p $> Intake
    let (_, runAmes) = ames env (fromIntegral point) True enqueue noStderr
    cb <- runAmes
    io (cb turfEf)
    pure (que, cb)
  where
    noStderr _ = pure ()

waitForPacket :: TQueue EvErr -> Bytes -> IO Bool
waitForPacket q val = go
 where
  go = atomically (readTQueue q) >>= \case
    EvErr (EvBlip (BlipEvNewt (NewtEvBorn (_, ()) ()))) _ -> go
    EvErr (EvBlip (BlipEvAmes (AmesEvHear () _ bs))) _ -> pure (bs == val)
    _ -> pure False

runRAcquire :: RAcquire e a -> RIO e a
runRAcquire acq = rwith acq pure

sendThread :: (NewtEf -> IO ()) -> (Galaxy, Bytes) -> RAcquire e ()
sendThread cb (to, val) = void $ mkRAcquire start cancel
  where
    start = async $ forever $ do threadDelay 1_000
                                 wen <- io $ now
                                 io $ cb (sendEf to wen val)
                                 threadDelay 10_000

zodSelfMsg :: Property
zodSelfMsg = forAll arbitrary (ioProperty . runNetworkApp . runTest)
 where
  runTest
    :: (HasLogFunc e, HasNetworkConfig e, HasKingId e) => Bytes -> RIO e Bool
  runTest val = runRAcquire $ do
    env         <- ask
    (zodQ, zod) <- runGala 0
    ()          <- sendThread zod (0, val)
    liftIO (waitForPacket zodQ val)

twoTalk :: Property
twoTalk = forAll arbitrary (ioProperty . runNetworkApp . runTest)
  where
    runTest :: (HasLogFunc e, HasNetworkConfig e, HasKingId e)
            => (Word8, Word8, Bytes) -> RIO e Bool
    runTest (aliceShip, bobShip, val) =
      if aliceShip == bobShip
        then pure True
        else go aliceShip bobShip val

    go :: (HasLogFunc e, HasNetworkConfig e, HasKingId e)
       => Word8 -> Word8 -> Bytes -> RIO e Bool
    go aliceShip bobShip val = runRAcquire $ do
        (aliceQ, alice) <- runGala aliceShip
        (bobQ,   bob)   <- runGala bobShip
        sendThread alice (Patp bobShip,   val)
        sendThread bob   (Patp aliceShip, val)
        liftIO (waitForPacket aliceQ val >> waitForPacket bobQ val)

tests :: TestTree
tests =
  testGroup "Ames"
    [ localOption (QuickCheckTests 10) $
          testProperty "Zod can send a message to itself" $
              zodSelfMsg

    -- TODO Why doesn't this work in CI?
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
  arbitrary = Ship <$> arb

instance Arbitrary LogIdentity where
  arbitrary = LogIdentity <$> arb <*> arb <*> arb
