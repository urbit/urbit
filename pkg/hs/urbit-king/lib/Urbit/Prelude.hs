{-|
    Convenient Re-Exports
-}

module Urbit.Prelude
    ( module ClassyPrelude
    , module Control.Arrow
    , module Control.Lens
    , module Data.Acquire
    , module Data.RAcquire
    , module Data.Void
    , module Urbit.Noun
    , module Text.Show.Pretty
    , module Text.Printf
    , module RIO
    , io, rio
    , logTrace
    , acquireWorker, acquireWorkerBound
    ) where

import ClassyPrelude
import Urbit.Noun

import Control.Arrow    ((<<<), (>>>))
import Control.Lens hiding (Each, Index, cons, index, snoc, uncons, unsnoc,
                            (<.>), (<|))
import Data.Acquire     (Acquire, mkAcquire, with)
import Data.RAcquire    (RAcquire, mkRAcquire, rwith)
import Data.RAcquire    (MonadAcquire(..), MonadRIO(..))
import Data.Void        (Void, absurd)
import Text.Printf      (printf)
import Text.Show.Pretty (pPrint, ppShow)

import RIO (RIO, runRIO)
import RIO (Utf8Builder, display, displayShow)
import RIO (threadDelay)
import RIO (HasLogFunc, LogFunc, LogLevel(..), logDebug, logError, logFuncL,
            logInfo, logOptionsHandle, logOther, logWarn, mkLogFunc,
            setLogMinLevel, setLogUseLoc, setLogUseTime, withLogFunc)

io :: MonadIO m => IO a -> m a
io = liftIO

rio :: MonadRIO m => RIO e a -> m e a
rio = liftRIO

logTrace :: HasLogFunc e => Utf8Builder -> RIO e ()
logTrace = logOther "trace"


-- Utils for Spawning Worker Threads -------------------------------------------

acquireWorker :: HasLogFunc e => Text -> RIO e () -> RAcquire e (Async ())
acquireWorker nam act = mkRAcquire (async act) kill
 where
  kill tid = do
    logInfo ("Killing worker thread: " <> display nam)
    cancel tid

acquireWorkerBound :: HasLogFunc e => Text -> RIO e () -> RAcquire e (Async ())
acquireWorkerBound nam act = mkRAcquire (asyncBound act) kill
 where
  kill tid = do
    logInfo ("Killing worker thread: " <> display nam)
    cancel tid

