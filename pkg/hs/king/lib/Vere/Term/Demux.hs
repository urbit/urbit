{-
    This allows multiple (zero or more) terminal clients to connect to
    the *same* logical arvo terminal. Terminals that connect will be
    given full event history since the creation of the demuxer.
-}

module Vere.Term.Demux (Demux, mkDemux, addDemux, useDemux) where

import UrbitPrelude

import Arvo          (Belt)
import Vere.Term.API (Client(Client))

import qualified Vere.Term.API   as Term
import qualified Vere.Term.Logic as Logic


-- External --------------------------------------------------------------------

data KeyedSet a = KeyedSet
    { _ksTable  :: IntMap a
    , _nextKey  :: Int
    }

instance Semigroup (KeyedSet a) where
    KeyedSet t1 k1 <> KeyedSet t2 k2 = KeyedSet (t1 <> t2) (max k1 k2)

instance Monoid (KeyedSet a) where
    mempty = KeyedSet mempty 0

ksInsertKey :: a -> KeyedSet a -> (Int, KeyedSet a)
ksInsertKey x (KeyedSet tbl nex) =
    (nex, KeyedSet (insertMap nex x tbl) (succ nex))

ksInsert :: a -> KeyedSet a -> KeyedSet a
ksInsert x s = snd $ ksInsertKey x s

ksDelete :: Int -> KeyedSet a -> KeyedSet a
ksDelete k (KeyedSet t n) = KeyedSet (deleteMap k t) n

--------------------------------------------------------------------------------

data Demux = Demux
    { dConns :: TVar (KeyedSet Client)
    , dStash :: TVar Logic.St
    }

mkDemux :: STM Demux
mkDemux = Demux <$> newTVar mempty <*> newTVar Logic.init

addDemux :: Client -> Demux -> STM ()
addDemux conn Demux{..} = do
    modifyTVar' dConns (ksInsert conn)
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
    for_ (_ksTable conns) $ \c -> Term.give c evs

{-
    Returns Nothing if any connected client disconnected. A `Demux`
    terminal lives forever, so you can continue to call this after it
    returns `Nothing`.

    If there are no attached clients, this will not return until one
    is attached.
-}
dTake :: Demux -> STM (Maybe Belt)
dTake Demux{..} = do
    conns <- readTVar dConns
    waitForBelt conns >>= \case
        (_, Just b ) -> pure (Just b)
        (k, Nothing) -> do writeTVar dConns (ksDelete k conns)
                           pure Nothing
  where
    waitForBelt :: KeyedSet Client -> STM (Int, Maybe Belt)
    waitForBelt ks = asum
                   $ fmap (\(k,c) -> (k,) <$> Term.take c)
                   $ mapToList
                   $ _ksTable ks
