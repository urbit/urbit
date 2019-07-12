{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE Strict, StrictData #-}

module Noun.Core
  ( Noun, pattern Cell, pattern Atom, nounSize
  ) where

import ClassyPrelude hiding (hash)

import Noun.Atom

import Data.Bits                 (xor)
import Data.Hashable             (hash)
import GHC.Natural               (Natural)
import GHC.Prim                  (reallyUnsafePtrEquality#)
import Test.QuickCheck.Arbitrary (Arbitrary(arbitrary))
import Test.QuickCheck.Gen       (Gen, scale, resize, getSize)


-- Types -----------------------------------------------------------------------

data Noun
    = NCell Int Word Noun Noun
    | NAtom Int Atom

{-# COMPLETE Cell, Atom #-}
pattern Cell x y <- NCell _ _ x y where Cell = mkCell
pattern Atom a   <- NAtom _ a     where Atom = mkAtom


--------------------------------------------------------------------------------

instance Hashable Noun where
  hash = \case NCell h _ _ _ -> h
               NAtom h _     -> h
  {-# INLINE hash #-}
  hashWithSalt = defaultHashWithSalt
  {-# INLINE hashWithSalt #-}

instance Eq Noun where
  (==) x y =
    case reallyUnsafePtrEquality# x y of
      1# -> True
      _  -> case (x, y) of
              (NAtom x1 a1, NAtom x2 a2) ->
                  x1 == x2 && a1 == a2
              (NCell x1 s1 h1 t1, NCell x2 s2 h2 t2) ->
                  s1==s2 && x1==x2 && h1==h2 && t1==t2
              _ ->
                  False
  {-# INLINE (==) #-}

instance Ord Noun where
  compare x y =
    case reallyUnsafePtrEquality# x y of
      1# -> EQ
      _  -> case (x, y) of
              (Atom _,     Cell _ _)   -> LT
              (Cell _ _,   Atom _)     -> GT
              (Atom a1,    Atom a2)    -> compare a1 a2
              (Cell h1 t1, Cell h2 t2) -> compare h1 h2 <> compare t1 t2
  {-# INLINE compare #-}


instance Arbitrary Noun where
  arbitrary = resize 1000 go
    where
      dub x = Cell x x
      go = do
        sz  <- getSize
        (bit, bat :: Bool) <- arbitrary
        case (sz, bit, bat) of
          ( 0, _,     _    ) -> Atom <$> genAtom
          ( _, False, _    ) -> Atom <$> genAtom
          ( _, True,  True ) -> dub <$> arbitrary
          ( _, True,  _    ) -> scale (\x -> x-10) (Cell <$> go <*> go)

genNatural :: Gen Natural
genNatural = fromInteger . abs <$> arbitrary

genAtom :: Gen Atom
genAtom = do
  arbitrary >>= \case
    False -> genNatural
    True  -> (`mod` 16) <$> genNatural

--------------------------------------------------------------------------------

{-# INLINE nounSize #-}
nounSize :: Noun -> Word
nounSize = \case
  NCell _ s _ _ -> s
  NAtom _ _     -> 1

{-# INLINE mkAtom #-}
mkAtom :: Atom -> Noun
mkAtom a = NAtom (hash a) a

{-# INLINE mkCell #-}
mkCell :: Noun -> Noun -> Noun
mkCell h t = NCell has siz h t
  where
    siz = nounSize h + nounSize t
    has = hash h `combine` hash t


-- Stolen from Hashable Library ------------------------------------------------

{-# INLINE combine #-}
combine :: Int -> Int -> Int
combine h1 h2 = (h1 * 16777619) `xor` h2

{-# INLINE defaultHashWithSalt #-}
defaultHashWithSalt :: Hashable a => Int -> a -> Int
defaultHashWithSalt salt x = salt `combine` hash x
