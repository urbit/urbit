{-# OPTIONS_GHC -Wwarn #-}

module Urbit.Ames where

import ClassyPrelude

import Data.IP
import Network.Socket
import Noun

import qualified Data.Vector as V
import qualified Urbit.Time  as Time
import qualified Vere.Ames   as VA

--------------------------------------------------------------------------------

data GalaxyInfo = GalaxyInfo { ip :: IPv4, age :: Time.Unix }

data Ames = Ames
  { live         :: Bool  -- ^ whether the listener is on
  , ourPort      :: Maybe Int
--  , threadId :: Thread
  , globalDomain :: Maybe Text  -- ^ something like "urbit.org"
  , imperial     :: V.Vector (Maybe GalaxyInfo)
  }

init :: Ames
init = Ames { live = False
            , ourPort = Nothing
            , globalDomain = Nothing
            , imperial = V.replicate 256 Nothing
            }

turf :: Ames -> [VA.Turf] -> IO Ames
turf ames []   = undefined
turf ames (VA.MkTurf turf:_) = do
  let t = mconcat (intersperse "." turf)

  pure ames{globalDomain = Just t}


data NetworkMode
  = LocalOnlyNetworking
  | GlobalNetworking

ioStart :: Ames -> NetworkMode -> Int -> Noun -> IO Ames
ioStart ames isLocal defaultPort (Cell _ _) = undefined
ioStart ames isLocal defaultPort (Atom who) = do
  let _port = if who < 256
              then computePort isLocal who
              else defaultPort

  -- TODO: set up another thread to own the recv socket, which makes the Ovums
  -- which get put into the computeQueue, like in _ames_recv_cb.
  withSocketsDo $ do
    s <- socket AF_INET Datagram 17
    -- bind s (SockAddrInet port )
    pure ()

  pure ames

computePort :: NetworkMode -> Atom -> Int
computePort LocalOnlyNetworking who = 31337 + (fromIntegral who)
computePort GlobalNetworking who    = 13337 + (fromIntegral who)
