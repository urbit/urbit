{-
   Behn is difficult to test. What properties should it have?

   Well, I can thinkg of a few:

     - Timers set for the past or present should fire immediately.

     - Timers set for the near future should fire and have minimal jitter.

     - Replaced and canceled timers do not fire.

     - If I setup a timer loop, I should expect a small but positive drift.

       - By "timer loop" I mean:

         - Set a timer for 1 second in the future.
         - Wait for it to fire.
         - Repeat.

    It's not easy to test these properties, but! We can generate behn
    "programs" and compare an idealised implementation against the real
    implementation.

    A behn "program" is series of commands:

        - Set a timer.
        - Unset the current timer.
        - Wait for a timer to fire.

    `pureBehn` is the idealised implementation. `realBehn` runs a
    program against the actual behn driver. `acceptable` determines
    whether or not the real results are close enough to the idealised
    results.
-}

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

import Control.Concurrent (runInBoundThread)
import Data.LargeWord     (LargeKey(..))
import Data.RAcquire      (runRAcquire)
import GHC.Natural        (Natural)
import KingApp            (inPierEnvRAcquire, runAppNoConfig)
import Network.Socket     (tupleToHostAddress)

import qualified Urbit.Time as Time
import qualified Vere.Log   as Log


-- Behn "Programs" -------------------------------------------------------------

data Cmd
    = Doze Word -- Set a timer for `n` ticks from the start time.
    | Kill      -- Cancel existing timer
    | Wait      -- Wait for a timer to fire and record the result
  deriving (Eq, Ord, Show)

newtype Prog = Prog { unProg :: [Cmd] }
  deriving newtype (Eq, Ord, Show)

{-
    All the times when a timer fired. Each number represents the number
    of ticks since the start of the execution.
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


-- Reference Implementation ----------------------------------------------------

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


-- Real Implementation ---------------------------------------------------------

μsTicks :: Iso' Integer Double
μsTicks = iso from to
  where
    from :: Integer -> Double
    from i = fromIntegral i / 10_000

    to :: Double -> Integer
    to d = floor (d * 10_000)

gapTicks :: Iso' Gap Double
gapTicks = microSecs . μsTicks

{-
    The result of actually executing the program.

    res -- List of times timers were fired.
    fir -- Queue of timer fires.
    wat -- Signal to terminate `Wait` command.
    kil -- Signal that the simulation has finished.
    top -- Simulation start time.

    killThread -- After 25 ticks, trigger the `kil` TMVar

    fireThread -- In a loop, waits for behn timer to fire and then:

        - Gets the current timer.
        - Calculates the number of ticks passed since the start time.
        - Adds that to the results list.
        - If the program is waiting for a timer to fire, wakes it up.

    ### Race Condition Hack

    There's a race condition here:

    - Right before the Wait command, we have a `Doze 0`.
    - That timer tiggers some logic asyncronously.
    - This thread continues on to the Wait command.
    - We setup the wait signal.
    - This happens before the other thread finishes.
    - That thread finds our wait signal and triggers it.
    - We stop waiting.
    - This leads to wrong behavior because:

        - That timer should have triggered "immediately".
        - The Wait command should block until the end
          of the simulation.
        - Instead the Wait command is satisfied by a
          timer that should not have satisfied it.

    What do?

        Just sleep for a quarter tick before starting the Wait flow.

    Nasty hack, but it works for now.
-}
realBehn :: Prog -> IO ProgRes
realBehn (Prog cmds) =
  runAppNoConfig $ runRAcquire $ inPierEnvRAcquire 0 True 0 $ do

    (res, fir, wat, kil) <- atomically $
        (,,,) <$> newTVar []
              <*> newTQueue
              <*> newTVar Nothing
              <*> newEmptyTMVar

    let tickGap     = 1 ^. from gapTicks
        simDuration = (tickGap * 25) ^. microSecs

    killThread <- io $ async $ do
        threadDelay $ fromIntegral simDuration
        atomically $ putTMVar kil ()

    top <- io now

    fireThread <- io $ async $ forever $ do
        atomically $ readTQueue fir
        wen <- now
        atomically $ do
            let dif = gap top wen ^. gapTicks
            readTVar wat >>= \case Nothing -> pure ()
                                   Just sg -> putTMVar sg ()
            modifyTVar res (dif:)

    IODrv _ runDrv <- behn (writeTQueue fir)
    runEf          <- runDrv

    let
        timeAfterNTicks :: Word -> Wen
        timeAfterNTicks = addGap top . view (from gapTicks) . fromIntegral

        finished = atomically (reverse <$> readTVar res)

        go = \case
            [] -> do
                atomically (takeTMVar kil)
                finished
            Kill : cs -> do
                runEf $ BehnEfDoze (0, ()) $ Nothing
                go cs
            Doze n : cs -> do
                runEf $ BehnEfDoze (0, ()) $ Just $ timeAfterNTicks n
                go cs
            Wait : cs -> do

                -- HACK Wait for quarter of a tick to resolve race condition.
                let quarterTick = (tickGap ^. microSecs) `div` 4
                threadDelay (fromIntegral quarterTick)

                sig <- atomically $ do res <- newEmptyTMVar
                                       writeTVar wat (Just res)
                                       pure res

                let wait = do takeTMVar sig
                              writeTVar wat Nothing

                join $ atomically $ asum [ takeTMVar kil $> finished
                                         , wait          $> go cs
                                         ]

    res <- io (go cmds)

    cancel fireThread
    cancel killThread

    pure res


-- Acceptance Criteria ---------------------------------------------------------

{-
    Is the test results and the real results within reasonable
    jitter expectations?

    Criteria:

        - Latency is always positives and always less than 1 tick.
        - 1-to-1 corresponance between timer fires from real and reference
          implementations.
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
            putStrLn "===="
            print ("INPU", pro)
            print ("FAKE", fak)
            print ("REAL", rel)
            putStrLn "===="
        pure god


-- Utils -----------------------------------------------------------------------

tests :: TestTree
tests =
    testGroup "Behn"
        [ localOption (QuickCheckTests 10) $
              testProperty "Real and reference are similar" $
                  behnVsRef
        ]
