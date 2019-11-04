{-
    This allows multiple (zero or more) terminal clients to connect to
    the *same* logical arvo terminal. Terminals that connect will be
    given full event history since the creation of the demuxer.
-}

module Vere.Drv.Term.Demux (Demux, mkDemux, addDemux, useDemux) where

import UrbitPrelude

import Arvo          (Belt)
import Vere.Drv.Term.API (Client(Client))

import qualified Vere.Drv.Term.API   as Term
import qualified Vere.Drv.Term.Logic as Logic


-- External --------------------------------------------------------------------

data Demux = Demux
    { dConns :: TVar [Client]
    , dStash :: TVar Logic.St
    }

mkDemux :: STM Demux
mkDemux = Demux <$> newTVar [] <*> newTVar Logic.init

addDemux :: Client -> Demux -> STM ()
addDemux conn Demux{..} = do
    modifyTVar' dConns (conn:)
    stash <- readTVar dStash
    Term.give conn (Logic.toTermEv <$> Logic.drawState stash)

useDemux :: Demux -> Client
useDemux d = Client { give = dGive d, take = dTake d }


-- Internal --------------------------------------------------------------------

steps :: [Term.Ev] -> Logic.St -> Logic.St
steps termEvs st = foldl' Logic.step st $ concat $ Logic.fromTermEv <$> termEvs

dGive :: Demux -> [Term.Ev] -> STM ()
dGive Demux{..} evs = do
    modifyTVar' dStash (force $ steps evs)
    conns <- readTVar dConns
    for_ conns $ \c -> Term.give c evs

dTake :: Demux -> STM Belt
dTake Demux{..} = do
    conns <- readTVar dConns
    asum (Term.take <$> conns)
