module BehnTests (tests) where

import Arvo
import Data.Acquire
import Data.Conduit
import Data.Conduit.List     hiding (take)
import Data.Ord.Unicode
import Ur.Noun
import Test.QuickCheck       hiding ((.&.))
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.TH
import Urbit.Time
import UrbitPrelude
import Vere.Behn
import Vere.Log
import Vere.Pier.Types

import Control.Concurrent (runInBoundThread, threadDelay)
import Data.LargeWord     (LargeKey(..))
import GHC.Natural        (Natural)
import King.App           (runApp)
import Network.Socket     (tupleToHostAddress)

import qualified Urbit.Time as Time
import qualified Vere.Log   as Log


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
