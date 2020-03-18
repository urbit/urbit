module Dashboard
  ( pattern FastAtom
  , pattern FastHint
  , Jet
  , Dashboard (match)
  , Freeboard
  , Hashboard
  , Fastboard
  , Careboard
  , runFree
  , runHash
  , runFast
  , runCare
  ) where

import ClassyPrelude

import Control.Monad.State.Strict

import SimpleNoun

type Jet = Noun -> Noun
type JetName = Atom
type Hash = Int

pattern FastAtom = 1953718630  -- %fast
pattern FastHint id n =
  C (A 11)
    (C
      (C (A FastAtom) (C (A 1) (A id)))
      n)

-- | A context in which to run nock which supports jet lookup.
class Monad m => Dashboard m where
  -- | Find the jet associated with the formula represented by the given noun,
  -- if any.
  match :: Noun -> m (Maybe Jet)

-- | A dashboard which doesn't jet.
newtype Freeboard a = Freeboard (Identity a)
  deriving newtype Functor
  deriving newtype Applicative
  deriving newtype Monad

-- | A dashboard which looks for jets by formula hash
newtype Hashboard a = Hashboard (Identity a)
  deriving newtype Functor
  deriving newtype Applicative
  deriving newtype Monad

-- | A dashboard which checks the head of formulas for "fast
-- hints" and uses the name contained in such a hint to look for jets.
newtype Fastboard a = Fastboard (Identity a)
  deriving newtype Functor
  deriving newtype Applicative
  deriving newtype Monad

-- | A dashboard which uses both lookup strategies, checking for consistency
-- between them and that each fast hint is applied to a unique formula.
-- Violations of these principles are written to standard out.
newtype Careboard a = Careboard (StateT (HashMap JetName Noun) IO a)
  deriving newtype Functor
  deriving newtype Applicative
  deriving newtype Monad

runFree :: Freeboard a -> a
runFree (Freeboard x) = runIdentity x

runHash :: Hashboard a -> a
runHash (Hashboard x) = runIdentity x

runFast :: Fastboard a -> a
runFast (Fastboard x) = runIdentity x

runCare :: Careboard a -> IO a
runCare (Careboard x) = evalStateT x mempty

instance Dashboard Freeboard where
  match _ = Freeboard $ pure Nothing

instance Dashboard Hashboard where
  match = Hashboard . pure . byHash . hash

instance Dashboard Fastboard where
  match = Fastboard . \case
    FastHint id n -> pure (byFast id)
    _ -> pure Nothing

-- TODO maybe also detect hash collisions
instance Dashboard Careboard where
  match = Careboard . \case
    n@(FastHint nm _) -> case namely nm of
      Just (h, j) -> do
        when (h /= hash n) $
          putStrLn ("careboard: jet " <> tshowA nm <> " should have its hash "
                    <> "updated from " <> tshow h <> " to " <> tshow (hash n))
        get <&> lookup nm >>= \case
          Just n' ->
            when (n' /= n) $
              putStrLn ("careboard: jet hint " <> tshowA nm <> " has been "
                        <> "detected on unequal formulae " <> tshow n
                        <> " and " <> tshow n' <> ", which is very bad")
          Nothing -> modify' (insertMap nm n)
        pure (Just j)
      Nothing -> do
        putStrLn ("careboard: unmatched fast hint: " ++ tshowA nm)
        pure $ byHash $ hash n
    n -> pure $ byHash $ hash n

byFast :: JetName -> Maybe Jet
byFast = flip lookup fast
  where
    fast :: HashMap JetName Jet
    fast = mapFromList $ map (\(n, _, j) -> (n, j)) jets

byHash :: Hash -> Maybe Jet
byHash = flip lookup hash
  where
    hash :: HashMap Hash Jet
    hash = mapFromList $ map (\(_, h, j) -> (h, j)) jets

namely :: JetName -> Maybe (Hash, Jet)
namely = flip lookup fash
  where
    fash :: HashMap JetName (Hash, Jet)
    fash = mapFromList $ map (\(n, h, j) -> (n, (h, j))) jets

tx = textToAtom

type Entry = (JetName, Hash, Jet)
-- | Your jets here
jets :: [Entry]
jets =
  [ (tx "dec", 1520491622440108403, \(A a) -> trace "jetting" $ A (a - 1))
  ]
