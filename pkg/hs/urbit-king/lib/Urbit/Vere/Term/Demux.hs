{-|
    This allows multiple (zero or more) terminal clients to connect to
    the *same* logical arvo terminal. Terminals that connect will be
    given full event history since the creation of the demuxer.
-}

module Urbit.Vere.Term.Demux (Demux,
                              mkDemux,
                              addDemux,
                              useDemux,
                              curDemuxSize) where

import Urbit.Prelude
import Urbit.TermSize
import Urbit.Vere.Term.API (Client(Client), ClientTake(..))

import qualified Urbit.Vere.Term.API   as Term
import qualified Urbit.Vere.Term.Logic as Logic


-- External --------------------------------------------------------------------

data KeyedSet a = KeyedSet
    { _ksTable :: IntMap a
    , _nextKey :: Int
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
    , dSizes :: TVar (IntMap TermSize)
    , dStash :: TVar Logic.St
    , dMinSize :: TVar TermSize
    }

mkDemux :: TermSize -> STM Demux
mkDemux ts = Demux <$>
  newTVar mempty <*>
  newTVar mempty <*>
  newTVar Logic.init <*>
  newTVar ts

addDemux :: Client -> Demux -> STM ()
addDemux conn Demux{..} = do
    modifyTVar' dConns (ksInsert conn)
    stash <- readTVar dStash
    Term.give conn (Logic.toTermEv <$> Logic.drawState stash)

useDemux :: Demux -> Client
useDemux d = Client { give = dGive d, take = dTake d }

curDemuxSize :: Demux -> STM TermSize
curDemuxSize Demux{..} = readTVar dMinSize

-- Internal --------------------------------------------------------------------

steps :: [Term.Ev] -> Logic.St -> Logic.St
steps termEvs st = foldl' Logic.step st $ concat $ Logic.fromTermEv <$> termEvs

dGive :: Demux -> [Term.Ev] -> STM ()
dGive Demux{..} evs = do
    modifyTVar' dStash (force $ steps evs)
    conns <- readTVar dConns
    for_ (_ksTable conns) $ \c -> Term.give c evs

{-|
    Returns Nothing if any connected client disconnected. A `Demux`
    terminal lives forever, so you can continue to call this after it
    returns `Nothing`.

    If there are no attached clients, this will not return until one
    is attached.
-}
dTake :: Demux -> STM (Maybe ClientTake)
dTake Demux{..} = do
    conns <- readTVar dConns
    waitForTake conns >>= \case
        (_, Just (ClientTakeBelt b)) -> pure (Just (ClientTakeBelt b))

        (k, Just (ClientTakeSize s)) -> do
          newSizeTree <- modifyAndReadTVar' dSizes (insertMap k s)
          maybeUpdateTerminalSize newSizeTree

        (k, Nothing) -> do
          writeTVar dConns (ksDelete k conns)
          newSizeTree <- modifyAndReadTVar' dSizes (deleteMap k)
          maybeUpdateTerminalSize newSizeTree

  where
    waitForTake :: KeyedSet Client -> STM (Int, Maybe ClientTake)
    waitForTake ks = asum
                   $ fmap (\(k,c) -> (k,) <$> Term.take c)
                   $ mapToList
                   $ _ksTable ks

    maybeUpdateTerminalSize :: IntMap TermSize -> STM (Maybe ClientTake)
    maybeUpdateTerminalSize newSizeTree = do
      let termSize = foldr minTermSize (TermSize 1024 1024) newSizeTree
      curSize <- readTVar dMinSize
      if curSize == termSize
        then pure Nothing
        else do
          writeTVar dMinSize termSize
          pure $ Just (ClientTakeSize termSize)

    modifyAndReadTVar' :: TVar a -> (a -> a) -> STM a
    modifyAndReadTVar' var fun = do
      pre <- readTVar var
      let !post = fun pre
      writeTVar var post
      pure post

    minTermSize :: TermSize -> TermSize -> TermSize
    minTermSize (TermSize wa ha) (TermSize wb hb) =
      TermSize (min wa wb) (min ha hb)
