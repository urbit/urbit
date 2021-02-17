{-|
  Scry helpers
-}

module Urbit.King.Scry
  ( scryNow
  , module Urbit.Vere.Pier.Types
  )
where

import Urbit.Prelude
import Urbit.Vere.Serf.Types

import Urbit.Arvo.Common (Desk)
import Urbit.Vere.Pier.Types (ScryFunc)

scryNow :: forall e n
        . (HasLogFunc e, FromNoun n)
       => ScryFunc
       -> Term    -- ^ vane + care as two-letter string
       -> Desk    -- ^ desk in scry path
       -> [Text]  -- ^ resource path to scry for
       -> RIO e (Maybe n)
scryNow scry vare desk path =
  io (scry Nothing (EachNo $ DemiOnce vare desk (Path $ MkKnot <$> path)))
    >>= \case
      Just ("omen", fromNoun @(Path, Term, n) -> Just (_,_,v)) -> pure $ Just v
      Just (_, fromNoun @n -> Just v) -> pure $ Just v
      Just (_, n) -> do
        logError $ displayShow ("uncanny scry result", vare, path, n)
        pure Nothing
      Nothing -> pure Nothing

