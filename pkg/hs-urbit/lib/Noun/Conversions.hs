module Noun.Conversions
  ( Cord(..), Knot(..), Term(..), Tank(..), Tang, Plum(..), Nullable
  ) where

import ClassyPrelude hiding (hash)

import Control.Lens
import Data.Void
import Data.Word
import Noun.Atom
import Noun.Convert
import Noun.Core
import Noun.TH

import GHC.Natural (Natural)
import RIO         (decodeUtf8Lenient)

import qualified Data.Char as C


-- TODO XX Hack! ---------------------------------------------------------------

instance Show Noun where
  show = \case Atom a   -> showAtom a
               Cell x y -> fmtCell (show <$> (x : toTuple y))
    where
      fmtCell :: [String] -> String
      fmtCell xs = "[" <> intercalate " " xs <> "]"

      toTuple :: Noun -> [Noun]
      toTuple (Cell x xs) = x : toTuple xs
      toTuple atom        = [atom]

      showAtom :: Atom -> String
      showAtom 0 = "0"
      showAtom a =
          let mTerm = do
                t <- fromNoun (Atom a)
                let ok = \x -> (x=='-' || C.isAlphaNum x)
                guard (all ok (t :: Text))
                pure ("%" <> unpack t)

          in case mTerm of
               Nothing -> show a
               Just st -> st


-- Noun ------------------------------------------------------------------------

instance ToNoun Noun where
  toNoun = id

instance FromNoun Noun where
  parseNoun = pure


-- Void ------------------------------------------------------------------------

instance ToNoun Void where
  toNoun = absurd

instance FromNoun Void where
  parseNoun = fail "Can't produce void"


-- Tour ------------------------------------------------------------------------

newtype Tour = Tour [Char]
  deriving (Eq, Ord, Show)


-- Atom or Cell ----------------------------------------------------------------

data AtomCell a c
    = ACAtom a
    | ACCell c
  deriving (Eq, Ord, Show)

instance (ToNoun a, ToNoun c) => ToNoun (AtomCell a c) where
  toNoun (ACAtom a) = toNoun a
  toNoun (ACCell c) = toNoun c

instance (FromNoun a, FromNoun c) => FromNoun (AtomCell a c) where
  parseNoun n = case n of
    Atom _   -> ACAtom <$> parseNoun n
    Cell _ _ -> ACCell <$> parseNoun n


-- Nullable --------------------------------------------------------------------

{-|
    `Nullable a <-> ?@(~ a)`

    This is distinct from `unit`, since there is no tag on the non-atom
    case, therefore `a` must always be cell type.
-}
type Nullable a = AtomCell () a


-- Char ------------------------------------------------------------------------

instance ToNoun Char where
  toNoun = toNoun . (fromIntegral :: Int -> Word32) . C.ord

instance FromNoun Char where
  parseNoun n = do
    w :: Word32 <- parseNoun n
    pure $ C.chr $ fromIntegral w


-- List ------------------------------------------------------------------------

instance ToNoun a => ToNoun [a] where
  toNoun xs = nounFromList (toNoun <$> xs)
    where
      nounFromList :: [Noun] -> Noun
      nounFromList []     = Atom 0
      nounFromList (x:xs) = Cell x (nounFromList xs)

instance FromNoun a => FromNoun [a] where
  parseNoun (Atom 0)   = pure []
  parseNoun (Atom _)   = fail "list terminated with non-null atom"
  parseNoun (Cell l r) = (:) <$> parseNoun l <*> parseNoun r


-- Tape ------------------------------------------------------------------------

-- TODO XX are these instances correct?
newtype Tape = Tape [Char]
  deriving newtype (Eq, Ord, Show, FromNoun, ToNoun)


-- Pretty Printing -------------------------------------------------------------

type Tang = [Tank]

type Tank = AtomCell Tape TankTree

data TankTree
    = Plum Plum
    | Palm (Tape, Tape, Tape, Tape) [Tank]
    | Rose (Tape, Tape, Tape) [Tank]
  deriving (Eq, Ord, Show)

data WideFmt = WideFmt { delimit :: Cord, enclose :: Maybe (Cord, Cord) }
  deriving (Eq, Ord, Show)

data TallFmt = TallFmt { intro :: Cord, indef :: Maybe (Cord, Cord) }
  deriving (Eq, Ord, Show)

data PlumFmt = PlumFmt (Maybe WideFmt) (Maybe TallFmt)
  deriving (Eq, Ord, Show)

type Plum = AtomCell Cord PlumTree

data PlumTree
    = Para Cord [Cord]
    | Tree PlumFmt [Plum]
    | Sbrk Plum
  deriving (Eq, Ord, Show)

deriveNoun ''WideFmt
deriveNoun ''TallFmt
deriveNoun ''PlumFmt
deriveNoun ''TankTree
deriveNoun ''PlumTree


-- ByteString ------------------------------------------------------------------

instance ToNoun ByteString where
  toNoun bs = toNoun (int2Word (length bs), bs ^. from atomBytes)
    where
      int2Word :: Int -> Word
      int2Word = fromIntegral

instance FromNoun ByteString where
    parseNoun x = do
        (word2Int -> len, atom) <- parseNoun x
        let bs = atom ^. atomBytes
        pure $ case compare (length bs) len of
          EQ -> bs
          LT -> bs <> replicate (len - length bs) 0
          GT -> take len bs
      where
        word2Int :: Word -> Int
        word2Int = fromIntegral


-- Text ------------------------------------------------------------------------

instance ToNoun Text where -- XX TODO
  toNoun t = toNoun (Cord (encodeUtf8 t))

instance FromNoun Text where -- XX TODO
  parseNoun n = do
    Cord c <- parseNoun n
    pure (decodeUtf8Lenient c)


-- Term ------------------------------------------------------------------------

newtype Term = MkTerm Text
  deriving newtype (Eq, Ord, Show)

instance ToNoun Term where -- XX TODO
  toNoun (MkTerm t) = toNoun (Cord (encodeUtf8 t))

instance FromNoun Term where -- XX TODO
  parseNoun n = do
    Cord c <- parseNoun n
    pure (MkTerm (decodeUtf8Lenient c))


-- Knot ------------------------------------------------------------------------

newtype Knot = MkKnot Text
  deriving newtype (Eq, Ord, Show)

instance ToNoun Knot where -- XX TODO
  toNoun (MkKnot t) = toNoun (Cord (encodeUtf8 t))

instance FromNoun Knot where -- XX TODO
  parseNoun n = do
    Cord c <- parseNoun n
    pure (MkKnot (decodeUtf8Lenient c))


-- Bool ------------------------------------------------------------------------

instance ToNoun Bool where
  toNoun True  = Atom 0
  toNoun False = Atom 1

instance FromNoun Bool where
  parseNoun (Atom 0)   = pure True
  parseNoun (Atom 1)   = pure False
  parseNoun (Cell _ _) = fail "expecting a bool, but got a cell"
  parseNoun (Atom a)   = fail ("expecting a bool, but got " <> show a)


-- Integer ---------------------------------------------------------------------

instance ToNoun Integer where
    toNoun = toNoun . (fromIntegral :: Integer -> Natural)

instance FromNoun Integer where
    parseNoun = fmap (fromIntegral :: Natural -> Integer) . parseNoun


-- Words -----------------------------------------------------------------------

atomToWord :: forall a. (Bounded a, Integral a) => Atom -> Parser a
atomToWord atom = do
  if atom > fromIntegral (maxBound :: a)
  then fail "Atom doesn't fit in fixed-size word"
  else pure (fromIntegral atom)

wordToNoun :: Integral a => a -> Noun
wordToNoun = Atom . fromIntegral

nounToWord :: forall a. (Bounded a, Integral a) => Noun -> Parser a
nounToWord = parseNoun >=> atomToWord

instance ToNoun Word    where toNoun = wordToNoun
instance ToNoun Word8   where toNoun = wordToNoun
instance ToNoun Word16  where toNoun = wordToNoun
instance ToNoun Word32  where toNoun = wordToNoun
instance ToNoun Word64  where toNoun = wordToNoun

instance FromNoun Word    where parseNoun = nounToWord
instance FromNoun Word8   where parseNoun = nounToWord
instance FromNoun Word16  where parseNoun = nounToWord
instance FromNoun Word32  where parseNoun = nounToWord
instance FromNoun Word64  where parseNoun = nounToWord


-- Maybe is `unit` -------------------------------------------------------------

-- TODO Consider enforcing that `a` must be a cell.
instance ToNoun a => ToNoun (Maybe a) where
  toNoun Nothing  = Atom 0
  toNoun (Just x) = Cell (Atom 0) (toNoun x)

instance FromNoun a => FromNoun (Maybe a) where
  parseNoun = \case
      Atom          0   -> pure Nothing
      Atom          n   -> unexpected ("atom " <> show n)
      Cell (Atom 0) t   -> Just <$> parseNoun t
      Cell n        _   -> unexpected ("cell with head-atom " <> show n)
    where
      unexpected s = fail ("Expected unit value, but got " <> s)


-- Tuple Conversions -----------------------------------------------------------

instance ToNoun () where
  toNoun () = Atom 0

instance FromNoun () where
  parseNoun (Atom 0) = pure ()
  parseNoun x        = fail ("expecting `~`, but got " <> show x)

instance (ToNoun a, ToNoun b) => ToNoun (a, b) where
  toNoun (x, y) = Cell (toNoun x) (toNoun y)

instance (FromNoun a, FromNoun b) => FromNoun (a, b) where
  parseNoun (Atom n)   = fail ("expected a cell, but got an atom: " <> show n)
  parseNoun (Cell l r) = (,) <$> parseNoun l <*> parseNoun r


instance (ToNoun a, ToNoun b, ToNoun c) => ToNoun (a, b, c) where
  toNoun (x, y, z) = toNoun (x, (y, z))

instance (FromNoun a, FromNoun b, FromNoun c) => FromNoun (a, b, c) where
  parseNoun n = do
    (x, t) <- parseNoun n
    (y, z) <- parseNoun t
    pure (x, y, z)

instance (ToNoun a, ToNoun b, ToNoun c, ToNoun d) => ToNoun (a, b, c, d) where
  toNoun (p, q, r, s) = toNoun (p, (q, r, s))

instance (FromNoun a, FromNoun b, FromNoun c, FromNoun d)
      => FromNoun (a, b, c, d)
      where
  parseNoun n = do
    (p, tail) <- parseNoun n
    (q, r, s) <- parseNoun tail
    pure (p, q, r, s)

instance (ToNoun a, ToNoun b, ToNoun c, ToNoun d, ToNoun e)
      => ToNoun (a, b, c, d, e) where
  toNoun (p, q, r, s, t) = toNoun (p, (q, r, s, t))

instance (FromNoun a, FromNoun b, FromNoun c, FromNoun d, FromNoun e)
      => FromNoun (a, b, c, d, e)
      where
  parseNoun n = do
    (p, tail)    <- parseNoun n
    (q, r, s, t) <- parseNoun tail
    pure (p, q, r, s, t)

instance (ToNoun a, ToNoun b, ToNoun c, ToNoun d, ToNoun e, ToNoun f)
      => ToNoun (a, b, c, d, e, f) where
  toNoun (p, q, r, s, t, u) = toNoun (p, (q, r, s, t, u))

instance ( FromNoun a, FromNoun b, FromNoun c, FromNoun d, FromNoun e
         , FromNoun f
         )
      => FromNoun (a, b, c, d, e, f)
      where
  parseNoun n = do
    (p, tail)       <- parseNoun n
    (q, r, s, t, u) <- parseNoun tail
    pure (p, q, r, s, t, u)

instance ( FromNoun a, FromNoun b, FromNoun c, FromNoun d, FromNoun e
         , FromNoun f, FromNoun g
         )
      => FromNoun (a, b, c, d, e, f, g)
      where
  parseNoun n = do
    (p, tail)          <- parseNoun n
    (q, r, s, t, u, v) <- parseNoun tail
    pure (p, q, r, s, t, u, v)

instance ( FromNoun a, FromNoun b, FromNoun c, FromNoun d, FromNoun e
         , FromNoun f, FromNoun g, FromNoun h
         )
      => FromNoun (a, b, c, d, e, f, g, h)
      where
  parseNoun n = do
    (p, tail)             <- parseNoun n
    (q, r, s, t, u, v, w) <- parseNoun tail
    pure (p, q, r, s, t, u, v, w)

instance ( FromNoun a, FromNoun b, FromNoun c, FromNoun d, FromNoun e
         , FromNoun f, FromNoun g, FromNoun h, FromNoun i
         )
      => FromNoun (a, b, c, d, e, f, g, h, i)
      where
  parseNoun n = do
    (p, tail)                <- parseNoun n
    (q, r, s, t, u, v, w, x) <- parseNoun tail
    pure (p, q, r, s, t, u, v, w, x)

instance ( FromNoun a, FromNoun b, FromNoun c, FromNoun d, FromNoun e
         , FromNoun f, FromNoun g, FromNoun h, FromNoun i, FromNoun j
         )
      => FromNoun (a, b, c, d, e, f, g, h, i, j)
      where
  parseNoun n = do
    (p, tail)                   <- parseNoun n
    (q, r, s, t, u, v, w, x, y) <- parseNoun tail
    pure (p, q, r, s, t, u, v, w, x, y)
