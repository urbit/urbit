{-# LANGUAGE NumericUnderscores, GeneralizedNewtypeDeriving #-}

module Urbit.Time where

import Prelude
import Control.Lens

import Data.Coerce            (coerce)
import Control.Concurrent     (threadDelay)
import Control.Exception      (throw, ArithException(Underflow))
import Data.Bits              (shiftL, shiftR)
import Data.LargeWord         (Word128, LargeKey(..))
import Data.Time.Clock        (DiffTime, UTCTime, picosecondsToDiffTime,
                               diffTimeToPicoseconds)
import Data.Time.Clock.System (SystemTime(..), getSystemTime, utcToSystemTime,
                               systemToUTCTime)


-- Types -----------------------------------------------------------------------

newtype Gap = Gap { unGap :: Word128 }
  deriving (Eq, Ord, Show)

newtype Wen = Wen { unWen :: Word128 }
  deriving (Eq, Ord, Show)

newtype Unix = Unix { unUnix :: Word128 }
  deriving (Eq, Ord, Show)


-- Basic Lenses ----------------------------------------------------------------

fractoSecs :: Iso' Gap Word128
fractoSecs = iso unGap Gap

sinceUrbitEpoch :: Iso' Wen Gap
sinceUrbitEpoch = iso (Gap . unWen) (Wen . unGap)

sinceUnixEpoch :: Iso' Unix Gap
sinceUnixEpoch = iso (Gap . unUnix) (Unix . unGap)


-- Instances -------------------------------------------------------------------

instance Num Gap where
  x + y       = Gap (coerce x + coerce y)
  x * y       = Gap (coerce x * coerce y)
  fromInteger = Gap . fromInteger
  abs         = over fractoSecs abs
  signum      = over fractoSecs signum
  negate      = over fractoSecs negate


-- Conversions -----------------------------------------------------------------

diffTime :: Iso' Gap DiffTime
diffTime = iso fromGap toGap
  where
    fromGap = picosecondsToDiffTime . view picoSecs
    toGap   = view (from picoSecs)  . diffTimeToPicoseconds

utcTime :: Iso' Unix UTCTime
utcTime = iso fromUnix toUnix
  where
    fromUnix = systemToUTCTime        . view systemTime
    toUnix   = view (from systemTime) . utcToSystemTime

wenUtcTime :: Iso' Wen UTCTime
wenUtcTime = unix . utcTime

systemTime :: Iso' Unix SystemTime
systemTime = iso toSys fromSys
  where
    toSys :: Unix -> SystemTime
    toSys (sinceUnixEpoch -> gap) =
      MkSystemTime (gap ^. secs) (gap ^. nanoSecs `mod` 1_000_000_000)

    fromSys :: SystemTime -> Unix
    fromSys (MkSystemTime numSecs ns) =
      fromUnixEpoch $ (numSecs ^. from secs) + (ns ^. from nanoSecs)

    fromUnixEpoch :: Gap -> Unix
    fromUnixEpoch (Gap g) = Unix g

    sinceUnixEpoch :: Unix -> Gap
    sinceUnixEpoch (Unix u) = Gap u

unixEpoch :: Wen
unixEpoch = Wen (LargeKey 0x8000_000c_ce9e_0d80 0)

unix :: Iso' Wen Unix
unix = iso toUnix fromUnix
  where
    fromUnix (Unix u)           = Wen (u + epoch)
    toUnix (Wen w) | w >= epoch = Unix (w - epoch)
                   | otherwise  = throw Underflow
    epoch                       = view (sinceUrbitEpoch . fractoSecs) unixEpoch

picoSecs :: (Integral a, Num a) => Iso' Gap a
picoSecs = iso fromGap toGap
  where
    fromGap (Gap x) = fromIntegral (shiftR (x * 1_000_000_000_000) 64)
    toGap x         = Gap (shiftL (fromIntegral x) 64 `div` 1_000_000_000_000)

nanoSecs :: (Integral a, Num a) => Iso' Gap a
nanoSecs = iso fromGap toGap
  where
    fromGap (Gap x) = fromIntegral (shiftR (x * 1_000_000_000) 64)
    toGap x         = Gap (shiftL (fromIntegral x) 64 `div` 1_000_000_000)

microSecs :: (Integral a, Num a) => Iso' Gap a
microSecs = iso fromGap toGap
  where
    fromGap (Gap x) = fromIntegral (shiftR (x * 1_000_000) 64)
    toGap x         = Gap (shiftL (fromIntegral x) 64 `div` 1_000_000)

milliSecs :: (Integral a, Num a) => Iso' Gap a
milliSecs = iso fromGap toGap
  where
    fromGap (Gap x) = fromIntegral (shiftR (x * 1_000) 64)
    toGap x         = Gap (shiftL (fromIntegral x) 64 `div` 1_000)

secs :: (Integral a, Num a) => Iso' Gap a
secs = iso fromGap toGap
  where
    fromGap (Gap x) = fromIntegral (shiftR x 64)
    toGap x         = Gap (shiftL (fromIntegral x) 64)


--------------------------------------------------------------------------------

now :: IO Wen
now = view (from systemTime . from unix) <$> getSystemTime

gap :: Wen -> Wen -> Gap
gap (Wen x) (Wen y) | x > y     = Gap (x - y)
                    | otherwise = Gap (y - x)

addGap :: Wen -> Gap -> Wen
addGap (Wen fs) (Gap g) = Wen (fs + g)

sleep :: Gap -> IO ()
sleep gap = threadDelay (gap ^. microSecs)

sleepUntil :: Wen -> IO ()
sleepUntil end = do
  now >>= \case
    start | start >= end -> pure ()
          | otherwise    -> sleep (gap start end)
