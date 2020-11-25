{-|
  Parsing of Ames packets
-}

module Urbit.Vere.Ames.Packet where

import Urbit.Prelude

import Control.Monad.Fail
import Data.Bits
import Data.LargeWord
import Data.Serialize

import Urbit.Arvo (AmesDest)

data Packet = Packet
  { pktVersion    :: Word8
  , pktEncrypted  :: Bool
  --
  , pktSndr       :: Ship
  , pktRcvr       :: Ship
  , pktOrigin     :: Maybe AmesDest
  , pktContent    :: Bytes
  }
  deriving Eq

instance Show Packet where
  show Packet {..}
    = "Packet {pktVersion = "
   <> show pktVersion
   <> ", pktEncrypted = "
   <> show pktEncrypted
   <> ", pktSndr = "
   <> show pktSndr
   <> ", pktRcvr = "
   <> show pktRcvr
   <> ", pktOrigin = "
   <> show pktOrigin
   <> ", pktContent = "
   <> showUD (bytesAtom $ unBytes pktContent)
   <> "}"

instance Serialize Packet where
  get = do
    -- header
    head <- getWord32le
    let pktVersion   =         head    .&. 0b111        & fromIntegral
    let checksum     = shiftR  head  3 .&. (2 ^ 20 - 1)
    let sndrRank     = shiftR  head 23 .&. 0b11
    let rcvrRank     = shiftR  head 25 .&. 0b11
    let pktEncrypted = testBit head 27  &  not  -- loobean
    -- verify checksum
    lookAhead $ do
      len  <- remaining
      body <- getBytes len
      let chk = fromIntegral (mugBS body) .&. (2 ^ 20 - 1)
      when (checksum /= chk) $
        fail ("checksum mismatch: expected " <> show checksum
           <> "; got " <> show chk)
    -- body
    pktSndr <- getShip sndrRank
    pktRcvr <- getShip rcvrRank
    len     <- remaining
    payload <- getBytes len
    -- data ("payload")
    (pktOrigin, pktContent) <- case cueBS payload of
          Left  e -> fail (show e)
          Right n -> case fromNounErr n of
            Left  e -> fail (show e)
            Right c -> pure c
    pure Packet {..}
    where
      getShip = fmap Ship . \case
        0 -> fromIntegral <$> getWord16le              -- galaxy / star
        1 -> fromIntegral <$> getWord32le              -- planet
        2 -> fromIntegral <$> getWord64le              -- moon
        3 -> LargeKey <$> getWord64le <*> getWord64le  -- comet
        _ -> fail "impossibiru"

  put Packet {..} = do
    let load = jamBS $ toNoun (pktOrigin, pktContent)
    let (sndR, putSndr) = putShipGetRank pktSndr
    let (rcvR, putRcvr) = putShipGetRank pktRcvr
    let body = runPut (putSndr <> putRcvr <> putByteString load)
    let chek = fromIntegral (mugBS body) .&. (2 ^ 20 - 1)
    let encr = pktEncrypted
    let vers = fromIntegral pktVersion .&. 0b111
    let head =        vers
           .|. shiftL chek                  3
           .|. shiftL sndR                 23
           .|. shiftL rcvR                 25
           .|. if     encr then 0 else bit 27
    putWord32le head
    putByteString body  -- XX can we avoid copy?
    where
      putShipGetRank s@(Ship (LargeKey p q)) = case () of
        _ | s < 2 ^ 16 -> (0, putWord16le $ fromIntegral s)    -- gar
          | s < 2 ^ 32 -> (1, putWord32le $ fromIntegral s)    -- pan
          | s < 2 ^ 64 -> (2, putWord64le $ fromIntegral s)    -- mon
          | otherwise  -> (3, putWord64le p >> putWord64le q)  -- com
