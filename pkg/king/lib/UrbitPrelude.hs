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
                     (<|))

import Control.Arrow    ((<<<), (>>>))
import Data.RAcquire    (RAcquire, mkRAcquire, rwith, withRIO, withRAcquire)
import Data.RAcquire    (MonadRIO(..), MonadAcquire(..))
import Data.Acquire     (Acquire, mkAcquire, with)
import Data.Void        (Void, absurd)
import Text.Printf      (printf)
import Text.Show.Pretty (pPrint, ppShow)

import RIO (RIO(RIO, unRIO), runRIO)
import RIO (Utf8Builder, display, displayShow)
import RIO (threadDelay)

import RIO ( HasLogFunc
           , LogFunc
           , logError
           , logInfo
           , logWarn
           , logDebug
           , logOther
           , logFuncL
           , logOptionsHandle
           , withLogFunc
           , setLogUseTime
           , setLogUseLoc
           )

io :: MonadIO m => IO a -> m a
io = liftIO

rio :: MonadRIO m => RIO e a -> m e a
rio = liftRIO

logTrace :: HasLogFunc e => Utf8Builder -> RIO e ()
logTrace = logOther "trace"
