module Urbit.Vere.Ames.LaneCache (cache) where

import Urbit.Prelude

import qualified Data.HashPSQ as P

import Urbit.Noun.Time

expiry :: Gap
expiry = (2 * 60) ^. from secs

-- | An "upside down" time for use as a priority.
newtype New = New Wen
  deriving newtype (Eq, Ord)

new = New . negate
wen (New w) = negate w

-- | Given a new, find an older new corresponding to `expiry` ago.
lag :: New -> New
lag n = new (addGap (wen n) expiry)

cache :: forall a b m n
       . (Ord a, Hashable a, MonadIO m, MonadIO n)
      => (a -> m b)
      -> n (a -> m b)
cache act = do
  cas <- newTVarIO (P.empty :: P.HashPSQ a New b)

  let fun x = P.lookup x <$> readTVarIO cas >>= \case
        Nothing -> thru
        Just (n, v) -> do
          let t = wen n
          t' <- io now
          if gap t' t > expiry
            then thru
            else pure v
        where
          -- Insert a key into the map, simultaneously removing *all* stale
          -- entries. Since insertion is linear in the size of the map,
          -- presumably it's not horrible to do it this way. The alternative
          -- would be to have a thread doing a purge every 10s or something, and
          -- then we'd have to be in RAcquire.
          up :: a -> New -> b -> P.HashPSQ a New b -> P.HashPSQ a New b
          up k n v ps = P.insert k n v
                      $ snd $ P.atMostView (lag n) ps
          thru :: m b
          thru = do
            n <- new <$> io now
            v <- act x
            atomically $ modifyTVar' cas $ up x n v
            pure v

  pure fun
