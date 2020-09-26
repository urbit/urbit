module Urbit.Vere.Ames.LaneCache (LaneCache, laneCache, byCache) where

import Urbit.Prelude

import Urbit.Noun.Time

expiry :: Gap
expiry = (2 * 60) ^. from secs

data LaneCache m a b = LaneCache
  { lcCache    :: TVar (Map a (Wen, b))
  , lcAction   :: a -> (b -> m ()) -> m ()
  }

laneCache :: (Ord a, MonadIO n)
          => (a -> (b -> m ()) -> m ())
          -> n (LaneCache m a b)
laneCache act = LaneCache <$> newTVarIO mempty <*> pure act

byCache :: (Ord a, MonadIO m)
        => LaneCache m a b
        -> a -> (b -> m ()) -> m ()
byCache LaneCache {..} x f = lookup x <$> readTVarIO lcCache >>= \case
  Nothing -> go
  Just (t, v) -> do
    t' <- io now
    if gap t' t > expiry
      then go
      else f v
  where
    go = lcAction x $ \v -> do
      t <- io now
      atomically $ modifyTVar' lcCache (insertMap x (t, v))
      f v
