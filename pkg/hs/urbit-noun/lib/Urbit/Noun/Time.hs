{-|
    TODO This is slow.
-}

module Urbit.Noun.Time where

import Control.Lens
import Prelude

import Data.Bits              (shiftL, shiftR, (.&.))
import Data.List              (intercalate)
import Data.Time.Calendar     (toGregorian)
import Data.Time.Clock        (DiffTime, UTCTime(..))
import Data.Time.Clock        (diffTimeToPicoseconds, picosecondsToDiffTime)
import Data.Time.Clock.System (SystemTime(..), getSystemTime)
import Data.Time.Clock.System (systemToUTCTime, utcToSystemTime)
import Data.Time.LocalTime    (TimeOfDay(..), timeToTimeOfDay)
import Data.Word              (Word64)
import Text.Printf            (printf)
import Urbit.Noun             (FromNoun, ToNoun)


-- Types -----------------------------------------------------------------------

newtype Gap = Gap { _fractoSecs :: Integer }
  deriving newtype (Eq, Ord, Show, Num, ToNoun, FromNoun)

newtype Unix = Unix { _sinceUnixEpoch :: Gap }
  deriving newtype (Eq, Ord, Show, ToNoun, FromNoun)

newtype Wen = Wen { _sinceUrbitEpoch :: Gap }
  deriving newtype (Eq, Ord, Show, Num, ToNoun, FromNoun)

newtype Date = MkDate { _dateWen :: Wen }
  deriving newtype (Eq, Ord, Num, ToNoun, FromNoun)


-- Record Lenses ---------------------------------------------------------------

makeLenses ''Gap
makeLenses ''Unix
makeLenses ''Wen
makeLenses ''Date


-- Instances -------------------------------------------------------------------

instance Show Date where
  show (MkDate wen) = if fs == 0
    then printf "~%i.%u.%u..%02u.%02u.%02u" y m d h min s
    else printf "~%i.%u.%u..%02u.%02u.%02u..%s" y m d h min s (showGap fs)
   where
    utc       = wen ^. systemTime . to systemToUTCTime
    (y, m, d) = toGregorian (utctDay utc)
    TimeOfDay h min (floor -> s::Int) = timeToTimeOfDay (utctDayTime utc)
    fs        = (wen ^. wenFracto . to (fromIntegral @Integer @Word64))

    wenFracto :: Lens' Wen Integer
    wenFracto = sinceUrbitEpoch . fractoSecs

    showGap :: Word64 -> String
    showGap gap = intercalate "." (printf "%04x" <$> bs)
     where
      bs = reverse $ dropWhile (== 0) [b4, b3, b2, b1]
      b4 = takeBits 16 gap
      b3 = takeBits 16 (shiftR gap 16)
      b2 = takeBits 16 (shiftR gap 32)
      b1 = takeBits 16 (shiftR gap 48)

    takeBits :: Int -> Word64 -> Word64
    takeBits wid wor = wor .&. (shiftL 1 wid - 1)


-- Conversion Lenses -----------------------------------------------------------

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

-- | Produce a Wen with precision compatible to that from vere's time.c
chop :: Wen -> Wen
chop (Wen (Gap g)) = Wen (Gap (mop g))
  where
      mop n = shiftL (shiftR n 48) 48
