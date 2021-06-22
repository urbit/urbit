module Urbit.Vere.Stat where

import Urbit.Prelude

data Stat = Stat
  { statAmes :: AmesStat
  }

data AmesStat = AmesStat
  { asUdp :: TVar Word
  , asUqf :: TVar Word
  , asUdf :: TVar Word
  , asUi6 :: TVar Word
  , asRcv :: TVar Word
  , asSup :: TVar Word
  , asSrf :: TVar Word
  , asQuf :: TVar Word
  , asFwd :: TVar Word
  , asDrt :: TVar Word
  , asDvr :: TVar Word
  , asDml :: TVar Word
  , asSwp :: TVar Word
  , asBal :: TVar Word
  , asOky :: TVar Word
  }

newStat :: MonadIO m => m Stat
newStat = do
  asUdp <- newTVarIO 0
  asUqf <- newTVarIO 0
  asUdf <- newTVarIO 0
  asUi6 <- newTVarIO 0
  asRcv <- newTVarIO 0
  asSup <- newTVarIO 0
  asSrf <- newTVarIO 0
  asQuf <- newTVarIO 0
  asFwd <- newTVarIO 0
  asDrt <- newTVarIO 0
  asDvr <- newTVarIO 0
  asDml <- newTVarIO 0
  asSwp <- newTVarIO 0
  asBal <- newTVarIO 0
  asOky <- newTVarIO 0
  pure Stat{statAmes = AmesStat{..}}

bump :: MonadIO m => TVar Word -> m ()
bump s = atomically $ bump' s

bump' :: TVar Word -> STM ()
bump' s = modifyTVar' s (+ 1)

type RenderedStat = [Text]

renderStat :: MonadIO m => Stat -> m RenderedStat
renderStat Stat{statAmes = AmesStat{..}} =
  sequence
    [ pure "stat:"
    , pure "  ames:"
    ,     ("    udp ingress:             " <>) <$> show <$> readTVarIO asUdp
    ,     ("    udp queue evict:         " <>) <$> show <$> readTVarIO asUqf
    ,     ("    udp recv fail:           " <>) <$> show <$> readTVarIO asUdf
    ,     ("    udp dropped non-ipv4:    " <>) <$> show <$> readTVarIO asUi6
    ,     ("    driver ingress:          " <>) <$> show <$> readTVarIO asRcv
    ,     ("    enqueued for serf:       " <>) <$> show <$> readTVarIO asSup
    ,     ("    sent to serf:            " <>) <$> show <$> readTVarIO asSrf
    ,     ("    serf queue evict:        " <>) <$> show <$> readTVarIO asQuf
    ,     ("    forwarded:               " <>) <$> show <$> readTVarIO asFwd
    ,     ("    dropped (unroutable):    " <>) <$> show <$> readTVarIO asDrt
    ,     ("    dropped (wrong version): " <>) <$> show <$> readTVarIO asDvr
    ,     ("    dropped (malformed):     " <>) <$> show <$> readTVarIO asDml
    ,     ("    serf swapped:            " <>) <$> show <$> readTVarIO asSwp
    ,     ("    serf bailed:             " <>) <$> show <$> readTVarIO asBal
    ,     ("    serf okay:               " <>) <$> show <$> readTVarIO asOky
    ]

