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
    ,     ("    udp ingress:             " <>) <$> tshow <$> readTVarIO asUdp
    ,     ("    udp queue evict:         " <>) <$> tshow <$> readTVarIO asUqf
    ,     ("    udp recv fail:           " <>) <$> tshow <$> readTVarIO asUdf
    ,     ("    udp dropped non-ipv4:    " <>) <$> tshow <$> readTVarIO asUi6
    ,     ("    driver ingress:          " <>) <$> tshow <$> readTVarIO asRcv
    ,     ("    enqueued for serf:       " <>) <$> tshow <$> readTVarIO asSup
    ,     ("    sent to serf:            " <>) <$> tshow <$> readTVarIO asSrf
    ,     ("    serf queue evict:        " <>) <$> tshow <$> readTVarIO asQuf
    ,     ("    forwarded:               " <>) <$> tshow <$> readTVarIO asFwd
    ,     ("    dropped (unroutable):    " <>) <$> tshow <$> readTVarIO asDrt
    ,     ("    dropped (wrong version): " <>) <$> tshow <$> readTVarIO asDvr
    ,     ("    dropped (malformed):     " <>) <$> tshow <$> readTVarIO asDml
    ,     ("    serf bailed:             " <>) <$> tshow <$> readTVarIO asBal
    ,     ("    serf okay:               " <>) <$> tshow <$> readTVarIO asOky
    ]

