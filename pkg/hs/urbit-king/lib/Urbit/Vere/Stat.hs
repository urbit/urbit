module Urbit.Vere.Stat where

import Urbit.Prelude

data Stat = Stat
  { statAmes :: AmesStat
  }

data AmesStat = AmesStat
  { asUdp :: TVar Word
  , asRcv :: TVar Word
  , asSwp :: TVar Word
  , asBal :: TVar Word
  , asOky :: TVar Word
  }

newStat :: MonadIO m => m Stat
newStat = do
  asUdp <- newTVarIO 0
  asRcv <- newTVarIO 0
  asSwp <- newTVarIO 0
  asBal <- newTVarIO 0
  asOky <- newTVarIO 0
  pure Stat{statAmes = AmesStat{..}}

type RenderedStat = [Text]

renderStat :: MonadIO m => Stat -> m RenderedStat
renderStat Stat{statAmes = AmesStat{..}} =
  sequence
    [ pure "stat:"
    , pure "  ames:"
    ,     ("    udp: " <>) <$> tshow <$> readTVarIO asUdp
    ,     ("    rcv: " <>) <$> tshow <$> readTVarIO asRcv
    ,     ("    swp: " <>) <$> tshow <$> readTVarIO asSwp
    ,     ("    bal: " <>) <$> tshow <$> readTVarIO asBal
    ,     ("    oky: " <>) <$> tshow <$> readTVarIO asOky
    ]

