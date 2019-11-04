module BehnTests (tests) where

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
import Vere.Drv.Behn
import Vere.Log
import Vere.Pier.Types

import Control.Concurrent (runInBoundThread, threadDelay)
import Data.LargeWord     (LargeKey(..))
import GHC.Natural        (Natural)
import KingApp            (runApp)
import Network.Socket     (tupleToHostAddress)

import qualified Urbit.Time as Time
import qualified Vere.Log   as Log


-- Better Property Test --------------------------------------------------------

{-
  How can we test Behn? What properties should a timer have?

  - Timers set for the past or present should fire immediately.

  - Timers set for the near future should fire and have minimal jitter.

  - Replaced and canceled timers do not fire.

  - If I setup a timer loop, I should expect a small but positive drift.

    - By "timer loop" I mean:

      - Set a timer for 1 second in the future.
      - Wait for it to fire.
      - Repeat.

  Is there a clever way to test this?

  I think so! Let's generate behn "programs" and compare their execution
  to a pure reference implementation. The simulation will run for, say,
  20ms. The generator for `Cmd` should almost always generate very
  small positive numbers, occasionally negative or zero, and rarely
  large positive numbers.
-}

data Cmd
    = Doze (Maybe Int) -- Cancel or set a timer for n ms from now.
    | Wait             -- Wait for a timer to fire and record the result

type Prog = [Cmd]

{-
    All the times when a timer fired. Each number represents the number
    of ms since the start of the execution.
-}
type ProgRes = [Double]

{-
    What the test would produce if timers had perfect precision
    and latency.
-}
pureBehn :: Prog -> ProgRes
pureBehn = fmap fromIntegral . go (0, Nothing, [])
  where
    go :: (Int, Maybe Int, [Int]) -> [Cmd] -> [Int]

    -- No timer and no commands
    go (_, Nothing, acc) [] = reverse acc

    -- Timer, but no commands
    go (_, Just n, acc) [] = reverse (n:acc)

    -- Wait command, but no timer
    go (_, Nothing, acc) (Wait:_) = reverse acc

    -- Wait command with timer set
    go (time, Just n, acc) (Wait:cs) = go (n, Nothing, n:acc) cs

    -- Timer canceled
    go (time, _, acc) (Doze Nothing : cs) = go (time, Nothing, acc) cs

    -- Timer set for present or past
    go (time, _, acc) (Doze (Just n) : cs) | n<1 =
        go (time, Nothing, time:acc) cs

    -- Timer set for the future
    go (time, _, acc) (Doze (Just n) : cs) =
        go (time, Just (time+n), acc) cs

{-
    The result of actually executing the program.
-}
realBehn :: Prog -> IO ProgRes
realBehn = undefined

{-
    Is the test results and the real results within reasonable
    jitter expectations?
-}
acceptable :: (ProgRes, ProgRes) -> Bool
acceptable = undefined


--------------------------------------------------------------------------------

king :: KingId
king = KingId 0

-- TODO Timers always fire immediatly. Something is wrong!
timerFires :: Property
timerFires = forAll arbitrary (ioProperty . runApp . runTest)
  where
    runTest :: HasLogFunc e => () -> RIO e Bool
    runTest () = do
        q <- newTQueueIO
        let IODrv _ run = behn king (writeTQueue q)
        rwith run $ \cb -> do
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
