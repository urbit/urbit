{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields    #-}

{-|
    Hoon's `map` and `set` types and conversions to/from Nouns.
-}
module Urbit.Noun.Tree
    ( HoonSet, setToHoonSet, setFromHoonSet
    , HoonMap, mapToHoonMap, mapFromHoonMap
    ) where

import ClassyPrelude
import Control.Lens  hiding (non)

import Urbit.Noun.Conversions ()
import Urbit.Noun.Convert
import Urbit.Noun.Core
import Urbit.Noun.TH


-- Types -----------------------------------------------------------------------

data NounVal a = NounVal
    { non :: !Noun
    , val :: !a
    }

data HoonTreeNode a = NTN
    { n :: !(NounVal a)
    , l :: !(HoonTree a)
    , r :: !(HoonTree a)
    }
  deriving (Eq, Ord, Show)

data HoonTree a = E | Node (HoonTreeNode a)
  deriving (Eq, Ord, Show)

pattern N :: NounVal a -> HoonTree a -> HoonTree a -> HoonTree a
pattern N n l r = Node (NTN n l r)

newtype HoonSet a = HoonSet { unHoonSet :: HoonTree a }
  deriving newtype (Eq, Ord, Show, FromNoun, ToNoun)

newtype HoonMap k v = HoonMap { unHoonMap :: HoonTree (k, v) }
  deriving newtype (Eq, Ord, Show, FromNoun, ToNoun)


-- Instances -------------------------------------------------------------------

instance Eq (NounVal a) where
  (==) = on (==) non

instance Ord (NounVal a) where
  compare = comparing non

instance ToNoun (NounVal a) where
  toNoun = non

instance Show a => Show (NounVal a) where
  show = show . val

instance FromNoun a => FromNoun (NounVal a) where
    parseNoun x = NounVal x <$> parseNoun x

instance ToNoun a => ToNoun (HoonTree a) where
    toNoun E        = A 0
    toNoun (Node n) = toNoun n

instance FromNoun a => FromNoun (HoonTree a) where
    parseNoun (A 0) = pure E
    parseNoun n     = Node <$> parseNoun n

deriveNoun ''HoonTreeNode


-- Order -----------------------------------------------------------------------

{-
    Orders in ascending double mug hash order, collisions fall back to dor.
-}
mor :: Noun -> Noun -> Bool
mor a b = if c == d then dor a b else c < d
  where
    c = mug $ A $ fromIntegral $ mug a
    d = mug $ A $ fromIntegral $ mug b

{-
    Orders in ascending tree depth.
-}
dor :: Noun -> Noun -> Bool
dor a       b       | a == b = True
dor (A a)   (C _ _)          = True
dor (C x y) (A b)            = False
dor (A a)   (A b)            = a < b
dor (C x y) (C p q) | x == p = dor y q
dor (C x y) (C p q)          = dor x p

{-
    Orders in ascending +mug hash order.

    Collisions fall back to dor.
-}
gor :: Noun -> Noun -> Bool
gor a b = if c==d then dor a b else c<d
  where (c, d) = (mug a, mug b)

morVal, gorVal :: NounVal a -> NounVal a -> Bool
morVal = on mor non
gorVal = on gor non


--------------------------------------------------------------------------------

nounVal :: ToNoun a => Iso' a (NounVal a)
nounVal = iso to val
  where
    to x = NounVal (toNoun x) x

treeToList :: forall a. HoonTree a -> [a]
treeToList = go []
  where
    go :: [a] -> HoonTree a -> [a]
    go acc = \case
        E                -> acc
        Node (NTN v l r) -> go (go (val v : acc) l) r

setFromHoonSet :: Ord a => HoonSet a -> Set a
setFromHoonSet = setFromList . treeToList . unHoonSet

mapFromHoonMap :: Ord k => HoonMap k v -> Map k v
mapFromHoonMap = mapFromList . treeToList . unHoonMap

setToHoonSet :: forall a. (Ord a, ToNoun a) => Set a -> HoonSet a
setToHoonSet = HoonSet . foldr put E . fmap (view nounVal) . setToList
  where
    put x = \case
        E                       -> N x E E
        Node a | x == n a       -> Node a
        Node a | gorVal x (n a) -> lef x a
        Node a                  -> rit x a

    rit x a = put x (r a) & \case
        E                           -> error "bad-put-set"
        Node c | morVal (n a) (n c) -> N (n a) (l a) (Node c)
        Node c                      -> N (n c) (N (n a) (l a) (l c)) (r c)

    lef x a = put x (l a) & \case
        E                           -> error "bad-put-set"
        Node c | morVal (n a) (n c) -> N (n a) (Node c) (r a)
        Node c                      -> N (n c) (l c) (N (n a) (r c) (r a))

p :: (ToNoun a, ToNoun b) => NounVal (a,b) -> NounVal a
p = view (from nounVal . to fst . nounVal)

pq :: (ToNoun a, ToNoun b) => NounVal (a,b) -> (NounVal a, NounVal b)
pq = boof . view (from nounVal)
  where
    boof (x, y) = (x ^. nounVal, y ^. nounVal)

mapToHoonMap :: forall k v. (ToNoun k, ToNoun v, Ord k, Ord v) => Map k v -> HoonMap k v
mapToHoonMap = HoonMap . foldr put E . fmap (view nounVal) . mapToList
  where
    put :: NounVal (k, v) -> HoonTree (k, v) -> HoonTree (k, v)
    put kv@(pq -> (b, c)) = \case
      E                            -> N kv E E
      Node a | kv == n a           -> Node a
      Node a | b  == p (n a)       -> N kv (l a) (r a)
      Node a | gorVal b (p $ n a)  -> lef kv a
      Node a                       -> rit kv a

    lef kv@(pq -> (b, c)) a = put kv (l a) & \case
      E                                   -> error "bad-put-map"
      Node d | morVal (p $ n a) (p $ n d) -> N (n a) (Node d) (r a)
      Node d                              -> N (n d) (l d) (N (n a) (r d) (r a))

    rit kv@(pq -> (b, c)) a = put kv (r a) & \case
      E                                   -> error "bad-put-map"
      Node d | morVal (p $ n a) (p $ n d) -> N (n a) (l a) (Node d)
      Node d                              -> N (n d) (N (n a) (l a) (l d)) (r d)
