{-
    This allows multiple (zero or more) terminal clients to connect to
    the *same* logical arvo terminal. Terminals that connect will be
    given full event history since the creation of the demuxer.
-}

module Vere.Term.Demux (Demux, mkDemux, addDemux, useDemux) where

import UrbitPrelude

import Arvo          (Belt)
import Vere.Term.API (Client(Client))

import qualified Vere.Term.API as Term


--------------------------------------------------------------------------------

data Demux = Demux
    { dConns :: TVar [Client]
    , dStash :: TVar [Term.Ev]
    }

mkDemux :: STM Demux
mkDemux = Demux <$> newTVar [] <*> newTVar []

addDemux :: Client -> Demux -> STM ()
addDemux conn Demux{..} = do
    stash <- readTVar dStash
    modifyTVar' dConns (conn:)
    for_ stash (Term.give conn)

useDemux :: Demux -> Client
useDemux d = Client { give = dGive d, take = dTake d }


-- Internal --------------------------------------------------------------------

dGive :: Demux -> Term.Ev -> STM ()
dGive Demux{..} ev = do
    modifyTVar' dStash (ev:)
    conns <- readTVar dConns
    for_ conns $ \c -> Term.give c ev

dTake :: Demux -> STM Belt
dTake Demux{..} = do
    conns <- readTVar dConns
    asum (Term.take <$> conns)
