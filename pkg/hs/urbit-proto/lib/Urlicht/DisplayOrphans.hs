module Urlicht.DisplayOrphans where

-- TODO make it not orphan when we move the higher-level infra from Deppy

import ClassyPrelude

import Bound
import Data.List (genericIndex)
import qualified Data.Text as T

import Deppy.RunicShow
import qualified Urlicht.Core as C
import qualified Urlicht.HoonToSimple as HS
import qualified Urlicht.Simple as S

instance RunicShow (C.Value Text) where
  runic = runic . C.quote

instance RunicShow (C.Core Text) where
  runic = runic . coreUp

instance RunicShow (S.Simple Text) where
  runic = runic . HS.up

displayMeta :: C.Meta -> Text
displayMeta = genericIndex names'
  where
    names' = fmap T.pack names
    names, letters :: [String]
    names = letters ++ (do n <- names; l <- letters; pure (n ++ l))
    letters = map (\x -> [x]) ['A'..'Z']

class InjectMeta a where
  injectMeta :: C.Meta -> a

instance InjectMeta Text where
  injectMeta = displayMeta

instance (InjectMeta a, Applicative f) => InjectMeta (f a) where
  injectMeta = pure . injectMeta

coreUp :: InjectMeta a => C.Core a -> S.Simple a
coreUp = go
  where
    go :: InjectMeta a => C.Core a -> S.Simple a
    go = \case
      C.Var x -> S.Var x
      C.Met m -> S.Var (injectMeta m)
      --
      C.Typ -> S.Typ
      C.Fun c sc -> S.Fun (go c) (hoistMeta go sc)
      --
      C.Lam sc -> S.Lam (hoistMeta go sc)
      --
      C.App c d -> S.App (go c) (go d)
      --
      C.Let c sc -> S.Let (go c) (hoistMeta go sc)

hoistMeta :: (Functor f, Applicative g, InjectMeta a)
          => (forall x. InjectMeta x => f x -> g x)
          -> Scope b f a
          -> Scope b g a
hoistMeta t (Scope s) = Scope $ t (fmap t <$> s)
