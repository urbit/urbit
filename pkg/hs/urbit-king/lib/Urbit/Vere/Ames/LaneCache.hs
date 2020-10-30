module Urbit.Vere.Ames.LaneCache (cache) where

import Urbit.Prelude

import Urbit.Noun.Time

expiry :: Gap
expiry = (2 * 60) ^. from secs

cache :: forall a b m n
       . (Ord a, MonadIO m, MonadIO n)
      => (a -> m b)
      -> n (a -> m b)
cache act = do
  cas <- newTVarIO (mempty :: Map a (Wen, b))

  let fun x = lookup x <$> readTVarIO cas >>= \case
        Nothing -> thru
        Just (t, v) -> do
          t' <- io now
          if gap t' t > expiry
            then thru
            else pure v
        where
          thru :: m b
          thru = do
            t <- io now
            v <- act x
            atomically $ modifyTVar' cas (insertMap x (t, v))
            pure v

  pure fun
