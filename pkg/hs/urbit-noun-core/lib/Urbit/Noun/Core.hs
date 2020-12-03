{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE Strict     #-}
{-# LANGUAGE StrictData #-}

{-|
    Core Noun Implementation

    Each cell has a pre-calculated hash and a `size` field. The size is
    the total number of nodes under the tree of the cell. This is used
    as a heuristic to choose a hash-table size for `jam` and `cue`.
-}
module Urbit.Noun.Core
  ( Noun, nounSize
  , pattern Cell, pattern Atom
  , pattern C, pattern A
  , textToUtf8Atom, utf8AtomToText
  , mug
  ) where

import ClassyPrelude hiding (hash)

import Urbit.Atom
import Urbit.Noun.Mug

import Data.Bits                 (xor)
import Data.Function             ((&))
import Data.Hashable             (hash)
import GHC.Natural               (Natural)
import GHC.Prim                  (reallyUnsafePtrEquality#)
import Test.QuickCheck.Arbitrary (Arbitrary(arbitrary))
import Test.QuickCheck.Gen       (Gen, getSize, resize, scale)

import qualified Data.Char as C


-- Types -----------------------------------------------------------------------

data Noun
    = NCell ~Mug Word Noun Noun
    | NAtom ~Mug Atom

pattern Cell :: Noun -> Noun -> Noun
pattern Atom :: Atom -> Noun

pattern Cell x y <- NCell _ _ x y where Cell = mkCell
pattern Atom a   <- NAtom _ a     where Atom = mkAtom

{-# COMPLETE Cell, Atom #-}

pattern C :: Noun -> Noun -> Noun
pattern A :: Atom -> Noun

pattern C x y <- NCell _ _ x y where C = mkCell
pattern A a   <- NAtom _ a     where A = mkAtom

{-# COMPLETE C, A #-}


--------------------------------------------------------------------------------

instance Hashable Noun where
  hash = fromIntegral . mug
  {-# INLINE hash #-}
  hashWithSalt salt x = salt `combine` hash x
  {-# INLINE hashWithSalt #-}

textToUtf8Atom :: Text -> Noun
textToUtf8Atom = Atom . utf8Atom

utf8AtomToText :: Noun -> Either Text Text
utf8AtomToText = \case
    Cell _ _ -> Left "Expected @t, but got ^"
    Atom atm -> atomUtf8 atm & \case
        Left err -> Left (tshow err)
        Right tx -> pure tx

instance Show Noun where
  show = \case Atom a   -> showAtom a
               Cell x y -> fmtCell (show <$> (x : toTuple y))
    where
      fmtCell :: [String] -> String
      fmtCell xs = "(" <> intercalate ", " xs <> ")"

      toTuple :: Noun -> [Noun]
      toTuple (Cell x xs) = x : toTuple xs
      toTuple atom        = [atom]

      showAtom :: Atom -> String
      showAtom 0 = "0"
      showAtom a | a >= 2^1024 = "\"...\""
      showAtom a =
          let mTerm = do
                t <- utf8AtomToText (Atom a)
                let ok = \x -> (C.isPrint x)
                if (all ok (t :: Text))
                    then pure ("\"" <> unpack t <> "\"")
                    else Left "Don't show as text."

          in case mTerm of
               Left _   -> show a
               Right st -> st

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

-- From http://hackage.haskell.org/package/hashable-1.2.7.0/docs/src/Data-Hashable-Class.html
combine :: Int -> Int -> Int
combine h1 h2 = (h1 * 16777619) `xor` h2

--------------------------------------------------------------------------------

{-# INLINE nounSize #-}
nounSize :: Noun -> Word
nounSize = \case
  NCell _ s _ _ -> s
  NAtom _ _     -> 1

{-# INLINE mug #-}
mug :: Noun -> Mug
mug = \case NCell h _ _ _ -> h
            NAtom h _     -> h

{-# INLINE mkAtom #-}
mkAtom :: Atom -> Noun
mkAtom a = NAtom (mugAtom a) a

{-# INLINE mkCell #-}
mkCell :: Noun -> Noun -> Noun
mkCell h t = NCell has siz h t
  where
    siz = nounSize h + nounSize t
    has = mugBoth (mug h) (mug t)
