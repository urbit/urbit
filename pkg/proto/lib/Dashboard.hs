module Dashboard where

import ClassyPrelude

import Control.Monad.State.Strict

import Noun

type Jet = Noun -> Noun
type Hash = Int

class Monad m => Dashboard m where
  register :: Atom -> Noun -> m ()
  match :: Noun -> m (Maybe Jet)

newtype Freeboard a = Freeboard (Identity a)
  deriving newtype Functor
  deriving newtype Applicative
  deriving newtype Monad

newtype Hashboard a = Hashboard (Identity a)
  deriving newtype Functor
  deriving newtype Applicative
  deriving newtype Monad

newtype Fastboard a = Fastboard (State (HashMap Noun Jet) a)
  deriving newtype Functor
  deriving newtype Applicative
  deriving newtype Monad

{-
newtype Careboard a = Careboard (State (HashMap Noun Jet) a)
  deriving newtype Functor
  deriving newtype Applicative
  deriving newtype Monad
-}

runFree :: Freeboard a -> a
runFree (Freeboard x) = runIdentity x

runHash :: Hashboard a -> a
runHash (Hashboard x) = runIdentity x

runFast :: Fastboard a -> a
runFast (Fastboard x) = evalState x mempty 

instance Dashboard Freeboard where
  register _ _ = pure ()
  match _ = pure Nothing

instance Dashboard Hashboard where
  register _ _ = pure ()
  match n = Hashboard $ pure $ byHash $ hash n

byFast :: Atom -> Maybe Jet
byFast = flip lookup fast
  where
    fast :: HashMap Atom Jet
    fast = mapFromList $ map (\(l, h, j) -> (l, j)) jets

byHash :: Hash -> Maybe Jet
byHash = flip lookup hash
  where
    hash :: HashMap Hash Jet
    hash = mapFromList $ map (\(l, h, j) -> (h, j)) jets

type Entry = (Atom, Hash, Jet)
jets :: [Entry]
jets =
  [
  ]
