{-# LANGUAGE NumericUnderscores, GeneralizedNewtypeDeriving #-}

-- TODO This is slow.

module Urbit.Time where

import Prelude
import Control.Lens

import Data.Bits              (shiftL, shiftR)
import Data.Time.Clock        (DiffTime, UTCTime, picosecondsToDiffTime,
                               diffTimeToPicoseconds)
import Data.Time.Clock.System (SystemTime(..), getSystemTime, utcToSystemTime,
                               systemToUTCTime)
import Data.Noun.Poet         (FromNoun, ToNoun)


-- Types -----------------------------------------------------------------------

newtype Gap = Gap { _fractoSecs :: Integer }
  deriving newtype (Eq, Ord, Show, Num, ToNoun, FromNoun)

newtype Unix = Unix { _sinceUnixEpoch :: Gap }
  deriving newtype (Eq, Ord, Show, ToNoun, FromNoun)

newtype Wen = Wen { _sinceUrbitEpoch :: Gap }
  deriving newtype (Eq, Ord, Show, ToNoun, FromNoun)


-- Lenses ----------------------------------------------------------------------

makeLenses ''Gap
makeLenses ''Unix
makeLenses ''Wen

diffTime :: Iso' Gap DiffTime
diffTime = iso fromGap toGap
  where
    fromGap = picosecondsToDiffTime . view picoSecs
    toGap   = view (from picoSecs)  . diffTimeToPicoseconds

sysUTC :: Iso' SystemTime UTCTime
sysUTC = iso systemToUTCTime utcToSystemTime

utcTime :: Iso' Wen UTCTime
utcTime = systemTime . sysUTC

unixEpoch :: Wen
unixEpoch = Wen (Gap 0x8000_000c_ce9e_0d80_0000_0000_0000_0000)

unixSystemTime :: Iso' Unix SystemTime
unixSystemTime = iso toSys fromSys
  where
    toSys (Unix gap) = MkSystemTime (fromInteger sec) (fromInteger ns)
      where (sec, ns) = quotRem (gap ^. nanoSecs) 1_000_000_000
    fromSys (MkSystemTime sec ns) =
      Unix $ (toInteger sec ^. from secs)
           + (toInteger ns  ^. from nanoSecs)

unix :: Iso' Wen Unix
unix = iso toUnix fromUnix
  where
    toUnix (Wen g)    = Unix (g - unWen unixEpoch)
    fromUnix (Unix g) = Wen (unWen unixEpoch + g)
    unWen (Wen x) = x

systemTime :: Iso' Wen SystemTime
systemTime = unix . unixSystemTime


--------------------------------------------------------------------------------

toDenomSecs :: Integer -> Gap -> Integer
toDenomSecs denom (Gap g) = shiftR (g * denom) 64

fromDenomSecs :: Integer -> Integer -> Gap
fromDenomSecs denom ds =
  Gap $ (shiftL ds 64) `div` denom

picoSecs :: Iso' Gap Integer
picoSecs = iso (toDenomSecs denom) (fromDenomSecs denom)
  where denom = 1_000_000_000_000

nanoSecs :: Iso' Gap Integer
nanoSecs = iso (toDenomSecs denom) (fromDenomSecs denom)
  where denom = 1_000_000_000

microSecs :: Iso' Gap Integer
microSecs = iso (toDenomSecs denom) (fromDenomSecs denom)
  where denom = 1_000_000

milliSecs :: Iso' Gap Integer
milliSecs = iso (toDenomSecs denom) (fromDenomSecs denom)
  where denom = 1_000

secs :: Iso' Gap Integer
secs = iso (toDenomSecs denom) (fromDenomSecs denom)
  where denom = 1


--------------------------------------------------------------------------------

now :: IO Wen
now = view (from systemTime) <$> getSystemTime

gap :: Wen -> Wen -> Gap
gap (Wen x) (Wen y) | x > y     = x - y
                    | otherwise = y - x

addGap :: Wen -> Gap -> Wen
addGap (Wen x) y = Wen (x+y)
