module BehnTests (tests) where

import Data.Acquire
import Data.Conduit
import Data.Conduit.List     hiding (take)
import Data.Ord.Unicode
import Test.QuickCheck       hiding ((.&.))
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.TH
import Urbit.Arvo
import Urbit.Noun
import Urbit.Noun.Time
import Urbit.Prelude
import Urbit.Vere.Behn
import Urbit.Vere.Pier.Types

import Control.Concurrent (runInBoundThread, threadDelay)
import Data.LargeWord     (LargeKey(..))
import GHC.Natural        (Natural)
import Network.Socket     (tupleToHostAddress)
import Urbit.King.App     (runKingEnvNoLog, HasKingId(..))

import qualified Urbit.Noun.Time     as Time


--------------------------------------------------------------------------------

-- TODO Timers always fire immediatly. Something is wrong!
timerFires :: Property
timerFires = forAll arbitrary (ioProperty . runKingEnvNoLog . runTest)
  where
    runTest :: HasKingId e => () -> RIO e Bool
    runTest () = do
      envr <- ask
      king <- fromIntegral <$> view kingIdL
      q <- newTQueueIO
      rwith (liftAcquire $ behn envr (writeTQueue q)) $ \cb -> do
        io $ cb (BehnEfDoze (king, ()) (Just (2^20)))
        t <- atomically $ readTQueue q
        pure True


-- Utils -----------------------------------------------------------------------

tests :: TestTree
tests =
  testGroup "Behn"
    [ localOption (QuickCheckTests 10) $
          testProperty "Behn Timers Fire" $
              timerFires
    ]
