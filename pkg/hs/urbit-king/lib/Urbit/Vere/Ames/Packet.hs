{-|
  Parsing of Ames packets
-}

module Urbit.Vere.Ames.Packet where

import Urbit.Prelude

import Control.Monad.Fail
import Data.Bits
import Data.LargeWord
import Data.List (genericIndex)
import Data.Serialize

import Urbit.Arvo (AmesAddress(..), Ipv4(..), Port(..))

data Packet = Packet
  { pktVersion  :: Word3
  , pktSndr     :: Ship
  , pktRcvr     :: Ship
  , pktSndrTick :: Word4
  , pktRcvrTick :: Word4
  , pktOrigin   :: Maybe AmesAddress
  , pktContent  :: ByteString
  }
  deriving Eq

instance Show Packet where
  show Packet {..}
    = "Packet {pktVersion = "
   <> show pktVersion
   <> ", pktSndr = "
   <> show pktSndr
   <> ", pktRcvr = "
   <> show pktRcvr
   <> ", pktSndrTick = "
   <> show pktSndrTick
   <> ", pktRcvrTick = "
   <> show pktRcvrTick
   <> ", pktOrigin = "
   <> show pktOrigin
   <> ", pktContent = "
   <> showUD (bytesAtom pktContent)
   <> "}"

{-
-- Wire format
data PacketHeader = PacketHeader
  { pktIsAmes    :: Bool       -- sim_o
  , pktVersion   :: Word3      -- ver_y
  , pktSndrClass :: ShipClass  -- sac_y
  , pktRcvrClass :: ShipClass  -- rac_y
  , pktChecksum  :: Word20     -- mug_l
  , pktIsRelayed :: Bool       -- rel_o
  }
  deriving Eq

data PacketBody = PacketBody
  { pktSndr     :: Ship               -- sen_d
  , pktRcvr     :: Ship               -- rec_d
  , pktSndrTick :: Word4              -- sic_y
  , pktRcvrTick :: Word4              -- ric_y
  , pktContent  :: ByteString         -- (con_s, con_y)
  , pktOrigin   :: Maybe AmesAddress  -- rog_d
  }
  deriving Eq
-}

type Word3 = Word8
type Word4 = Word8
type Word20 = Word32

data ShipClass
  = Lord
  | Planet
  | Moon
  | Comet
  deriving (Eq, Show)

muk :: ByteString -> Word20
muk bs = mugBS bs .&. (2 ^ 20 - 1)

putAmesAddress :: Putter AmesAddress
putAmesAddress = \case
  AAIpv4 (Ipv4 ip) (Port port) -> putWord32le ip >> putWord16le port

instance Serialize Packet where
  get = do
    -- header
    head <- getWord32le
    -- skip first three bits
    let isAmes     = testBit head  3  &  not
    let pktVersion = shiftR  head  4 .&. 0b111 & fromIntegral
    let sndrRank   = shiftR  head  7 .&. 0b11
    let rcvrRank   = shiftR  head  9 .&. 0b11
    let checksum   = shiftR  head 11 .&. (2 ^ 20 - 1)
    let isRelayed  = testBit head 31  &  not  -- loobean
    let sndrClass = genericIndex [Lord, Planet, Moon, Comet] sndrRank
    let rcvrClass = genericIndex [Lord, Planet, Moon, Comet] rcvrRank
    guard isAmes

    pktOrigin <- if isRelayed
      then Just <$> get
      else pure Nothing

    -- body
    lookAhead $ do
      len <- remaining
      body <- getBytes len
      let chk = muk body
      when (checksum /= chk) $
        fail ("checksum mismatch: expected " <> show checksum
           <> "; got " <> show chk)

    tick <- getWord8
    let pktSndrTick = tick .&. 0b1111
    let pktRcvrTick = shiftR tick 4

    pktSndr <- getShip sndrClass
    pktRcvr <- getShip rcvrClass

    len <- remaining
    pktContent <- getBytes len

    pure Packet{..}
    where
      getShip = fmap Ship . \case
        Lord   -> fromIntegral <$> getWord16le
        Planet -> fromIntegral <$> getWord32le
        Moon   -> fromIntegral <$> getWord64le
        Comet  -> LargeKey <$> getWord64le <*> getWord64le

  put Packet{..} = do
    let (sndR, putSndr) = putShipGetRank pktSndr
    let (rcvR, putRcvr) = putShipGetRank pktRcvr

    let body = runPut $ do
          putWord8 $ (pktSndrTick .&. 0b1111)
                 .|. shiftL (pktRcvrTick .&. 0b1111) 4
          putSndr
          putRcvr
          putByteString pktContent

    let vers = fromIntegral pktVersion .&. 0b111
    let chek = muk body

    -- skip first 3 bytes, set 4th to yes (0) for "is ames"
    let head = shiftL vers 4
           .|. shiftL sndR 7
           .|. shiftL rcvR 9
           .|. shiftL chek 11
           .|. if isJust pktOrigin then 0 else bit 31

    putWord32le head
    case pktOrigin of
      Just o  -> put o
      Nothing -> pure ()
    putByteString body
    
    where
      putShipGetRank s@(Ship (LargeKey p q)) = case () of
        _ | s < 2 ^ 16 -> (0, putWord16le $ fromIntegral s)    -- lord
          | s < 2 ^ 32 -> (1, putWord32le $ fromIntegral s)    -- planet
          | s < 2 ^ 64 -> (2, putWord64le $ fromIntegral s)    -- moon
          | otherwise  -> (3, putWord64le p >> putWord64le q)  -- comet
