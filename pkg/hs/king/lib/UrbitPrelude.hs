module UrbitPrelude
    ( module ClassyPrelude
    , module Control.Arrow
    , module Control.Lens
    , module Data.Acquire
    , module Data.RAcquire
    , module Data.Void
    , module Noun
    , module Text.Show.Pretty
    , module Text.Printf
    , module RIO
    , io, rio
    , logTrace
    ) where

import ClassyPrelude
import Noun

import Control.Lens hiding (Index, cons, index, snoc, uncons, unsnoc, (<.>),
                     (<|), Each)

import Control.Arrow    ((<<<), (>>>))
import Data.Acquire     (Acquire, mkAcquire, with)
import Data.RAcquire    (RAcquire, mkRAcquire, rwith)
import Data.RAcquire    (MonadAcquire(..), MonadRIO(..))
import Data.Void        (Void, absurd)
import Text.Printf      (printf)
import Text.Show.Pretty (pPrint, ppShow)

import RIO (RIO, runRIO)
import RIO (Utf8Builder, display, displayShow)
import RIO (threadDelay)

import RIO (HasLogFunc, LogFunc, logDebug, logError, logFuncL, logInfo,
            logOptionsHandle, logOther, logWarn, mkLogFunc, setLogUseLoc,
            setLogUseTime, withLogFunc)

io :: MonadIO m => IO a -> m a
io = liftIO

rio :: MonadRIO m => RIO e a -> m e a
rio = liftRIO

logTrace :: HasLogFunc e => Utf8Builder -> RIO e ()
logTrace = logOther "trace"
