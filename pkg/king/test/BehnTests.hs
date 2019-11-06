module BehnTests where

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

import Control.Concurrent (runInBoundThread)
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
    = Doze Word -- Set a timer for `n` ms from the start time.
    | Kill      -- Cancel existing timer
    | Wait      -- Wait for a timer to fire and record the result
  deriving (Eq, Ord, Show)

newtype Prog = Prog { unProg :: [Cmd] }
  deriving newtype (Eq, Ord, Show)

{-
    All the times when a timer fired. Each number represents the number
    of ms since the start of the execution.
-}
type ProgRes = [Double]

instance Arbitrary Cmd where
    arbitrary = oneof (set <> unset <> wait)
      where
        wait  = [pure Wait]
        unset = [pure Kill]
        set   = pure . Doze <$> [0..19]

instance Arbitrary Prog where
    arbitrary = Prog . take 20 <$> arbitrary

{-
    What the test would produce if timers had perfect precision
    and latency.
-}
pureBehn :: Prog -> ProgRes
pureBehn = fmap fromIntegral . go (0, Nothing, []) . unProg
  where
    go :: (Word, Maybe Word, [Word]) -> [Cmd] -> [Word]

    {- No timer and no commands -}
    go (_, Nothing, acc) [] =
        reverse acc

    {- Timer, but no commands -}
    go (_, Just n, acc) [] =
        reverse (n:acc)

    {- Wait command, but no timer -}
    go (_, Nothing, acc) (Wait:_) =
        reverse acc

    {- Wait command with timer set -}
    go (t, Just n, acc) (Wait:cs) =
        go (n, Nothing, n:acc) cs

    {- Timer canceled -}
    go (t, _, acc) (Kill : cs) =
        go (t, Nothing, acc) cs

    {- Timer set -}
    go (t, _, acc) (Doze n : cs) =
        if n <= t
        then go ( t, Nothing, t:acc ) cs
        else go ( t, Just n,  acc   ) cs

runRAcquire :: (MonadUnliftIO (m e),  MonadIO (m e), MonadReader e (m e))
            => RAcquire e a -> m e a
runRAcquire act = rwith act pure

gapToFloatingMs :: Gap -> Double
gapToFloatingMs = (/ 3000) . fromIntegral . view microSecs

{-
    The result of actually executing the program.
-}
realBehn :: Prog -> IO ProgRes
realBehn (Prog cmds) = runApp $ runRAcquire $ do

    {- Recorded times when behn pushed events -}
    res <- newTVarIO ([] :: [Double])

    {- Signal that a timer fired, and signal to stop `Wait` command -}
    fir <- newEmptyTMVarIO
    wat <- newEmptyTMVarIO

    let onWake _ = do
            emp <- isEmptyTMVar wat
            when emp (putTMVar wat ())
            putTMVar fir ()

    {- Signal that 75ms has passed -}
    kil <- newEmptyTMVarIO
    void $ io $ async (threadDelay (25 * 3_000) >> atomically (putTMVar kil ()))

    {- Execution start time -}
    top <- io now

    {- Calculate ms since start time -}
    let atTime :: Word -> Wen
        atTime n = addGap top (fromIntegral (3*n) ^. from milliSecs)

    {- Add to result every time a timer fires -}
    void $ io $ async $ forever $ do
        () <- atomically (takeTMVar fir)
        wen <- now
        dif <- pure $ gapToFloatingMs $ gap top wen
        atomically $ do
            acc <- readTVar res
            writeTVar res (dif:acc)

    let IODrv _ runDrv = behn 0 onWake
    cb <- runDrv

    let exit = atomically (reverse <$> readTVar res)

    let go [] = do
            atomically (takeTMVar kil)
            exit
        go (c:cs) = c & \case
          Kill -> do
              cb (BehnEfDoze (0, ()) $ Nothing)
              go cs
          Doze n -> do
              cb (BehnEfDoze (0, ()) $ Just $ atTime n)
              go cs
          Wait -> do
              join $ atomically $ asum
                  [ takeTMVar kil >> pure exit
                  , takeTMVar wat >> pure (go cs)
                  ]

    rio (go cmds)

{-
    Is the test results and the real results within reasonable
    jitter expectations?
-}
acceptable :: ProgRes -> ProgRes -> Bool
acceptable rel fak = go rel fak
  where
    go []     []     = True
    go []     _      = False
    go _      []     = False
    go (r:rs) (f:fs) = and [ r > f
                           , r-f < 1
                           , go rs fs
                           ]


--------------------------------------------------------------------------------

behnVsRef :: Property
behnVsRef = forAll arbitrary (ioProperty . runTest)
  where
    runTest :: Prog -> IO Bool
    runTest pro = do
        rel <- realBehn pro
        fak <- pure (pureBehn pro)
        god <- pure (acceptable rel fak)
        when (not god) $ do
            print ("INPU", pro)
            print ("FAKE", fak)
            print ("REAL", rel)
        pure god

-- Utils -----------------------------------------------------------------------

tests :: TestTree
tests =
    testGroup "Behn"
        [ localOption (QuickCheckTests 10) $
              testProperty "Real and reference are similar" $
                  behnVsRef
        ]
