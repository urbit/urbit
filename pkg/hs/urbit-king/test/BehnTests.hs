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
import Urbit.Prelude
import Urbit.Time
import Urbit.Vere.Behn
import Urbit.Vere.Log
import Urbit.Vere.Pier.Types

import Control.Concurrent (runInBoundThread, threadDelay)
import Data.LargeWord     (LargeKey(..))
import GHC.Natural        (Natural)
import Network.Socket     (tupleToHostAddress)
import Urbit.King.App     (runApp)

import qualified Urbit.Time     as Time
import qualified Urbit.Vere.Log as Log


--------------------------------------------------------------------------------

king :: KingId
king = KingId 0

-- TODO Timers always fire immediatly. Something is wrong!
timerFires :: Property
timerFires = forAll arbitrary (ioProperty . runApp . runTest)
  where
    runTest :: () -> RIO e Bool
    runTest () = do
      q <- newTQueueIO
      rwith (liftAcquire $ snd $ behn king (writeTQueue q)) $ \cb -> do
        cb (BehnEfDoze (king, ()) (Just (2^20)))
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
