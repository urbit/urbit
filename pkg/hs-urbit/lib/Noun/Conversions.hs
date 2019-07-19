module Noun.Conversions
  ( Nullable(..), Jammed(..), AtomCell(..)
  , Word128, Word256, Word512
  , Cord(..), Knot(..), Term(..), Tape(..), Tour(..)
  , Tank(..), Tang, Plum(..)
  , Mug(..), Path(..), Ship(..)
  ) where

import ClassyPrelude hiding (hash)

import Control.Lens
import Data.Void
import Data.Word
import Noun.Atom
import Noun.Convert
import Noun.Core
import Noun.TH

import Data.LargeWord (LargeKey, Word128, Word256)
import GHC.Natural    (Natural)
import Noun.Cue       (cue)
import Noun.Jam       (jam)
import RIO            (decodeUtf8Lenient)

import qualified Data.Char as C


-- TODO XX Hack! ---------------------------------------------------------------

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
      showAtom 0               = "()"
      showAtom a | a >= 2^1024 = "\"...\""
      showAtom a =
          let mTerm = do
                t <- fromNoun (Atom a)
                let ok = \x -> (x=='-' || C.isAlphaNum x)
                guard (all ok (t :: Text))
                pure ("\"" <> unpack t <> "\"")

          in case mTerm of
               Nothing -> show a
               Just st -> st


-- Noun ------------------------------------------------------------------------

instance ToNoun Noun where
  toNoun = id

instance FromNoun Noun where
  parseNoun = named "Noun" . pure


-- Void ------------------------------------------------------------------------

instance ToNoun Void where
  toNoun = absurd

instance FromNoun Void where
  parseNoun _ = named "Void" $ fail "Can't produce void"


-- Tour ------------------------------------------------------------------------

newtype Tour = Tour [Char]
  deriving newtype (Eq, Ord, Show, ToNoun, FromNoun)


-- Double Jammed ---------------------------------------------------------------

newtype Jammed a = Jammed a
  deriving (Eq, Ord, Show)

instance ToNoun a => ToNoun (Jammed a) where
  toNoun (Jammed a) = Atom $ jam $ toNoun a

instance FromNoun a => FromNoun (Jammed a) where
  parseNoun n = named "Jammed" $ do
    a <- parseNoun n
    cue a & \case
      Left err  -> fail (show err)
      Right res -> do
        Jammed <$> parseNoun res


-- Atom or Cell ----------------------------------------------------------------

type Word512 = LargeKey Word256 Word256

data AtomCell a c
    = ACAtom a
    | ACCell c
  deriving (Eq, Ord, Show)

instance (ToNoun a, ToNoun c) => ToNoun (AtomCell a c) where
  toNoun (ACAtom a) = toNoun a
  toNoun (ACCell c) = toNoun c

instance (FromNoun a, FromNoun c) => FromNoun (AtomCell a c) where
  parseNoun n = named "(,)" $ case n of
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
  parseNoun n = named "Char" $ do
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
  parseNoun = named "[]" . \case
      Atom 0   -> pure []
      Atom _   -> fail "list terminated with non-null atom"
      Cell l r -> (:) <$> parseNoun l <*> parseNoun r


-- Tape ------------------------------------------------------------------------

-- TODO XX are these instances correct?
newtype Tape = Tape [Char]
  deriving newtype (Eq, Ord, Show, FromNoun, ToNoun)


-- Pretty Printing -------------------------------------------------------------

type Tang = [Tank]

data Tank
    = Leaf Tape
    | Plum Plum
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
deriveNoun ''Tank
deriveNoun ''PlumTree


-- ByteString ------------------------------------------------------------------

instance ToNoun ByteString where
  toNoun bs = toNoun (int2Word (length bs), bs ^. from atomBytes)
    where
      int2Word :: Int -> Word
      int2Word = fromIntegral

instance FromNoun ByteString where
    parseNoun x = named "ByteString" $ do
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
  parseNoun n = named "Text" $ do
    Cord c <- parseNoun n
    pure (decodeUtf8Lenient c)


-- Term ------------------------------------------------------------------------

newtype Term = MkTerm Text
  deriving newtype (Eq, Ord, Show)

instance ToNoun Term where -- XX TODO
  toNoun (MkTerm t) = toNoun (Cord (encodeUtf8 t))

instance FromNoun Term where -- XX TODO
  parseNoun n = named "Term" $ do
    Cord c <- parseNoun n
    pure (MkTerm (decodeUtf8Lenient c))


-- Knot ------------------------------------------------------------------------

newtype Knot = MkKnot Text
  deriving newtype (Eq, Ord, Show, Semigroup, Monoid, IsString)

instance ToNoun Knot where -- XX TODO
  toNoun (MkKnot t) = toNoun (Cord (encodeUtf8 t))

instance FromNoun Knot where -- XX TODO
  parseNoun n = named "Knot" $ do
    Cord c <- parseNoun n
    pure (MkKnot (decodeUtf8Lenient c))


-- Ship ------------------------------------------------------------------------

newtype Ship = Ship Word128 -- @p
  deriving newtype (Eq, Ord, Show, Num, ToNoun, FromNoun)


-- Path ------------------------------------------------------------------------

newtype Path = Path [Knot]
  deriving newtype (Eq, Ord, Semigroup, Monoid, ToNoun, FromNoun)

instance Show Path where
  show (Path ks) = show $ intercalate "/" ("" : ks)


-- Mug -------------------------------------------------------------------------

newtype Mug = Mug Word32
  deriving newtype (Eq, Ord, Show, Num, ToNoun, FromNoun)


-- Bool ------------------------------------------------------------------------

instance ToNoun Bool where
  toNoun True  = Atom 0
  toNoun False = Atom 1

instance FromNoun Bool where
  parseNoun = named "Bool" . parse
    where
      parse n =
        parseNoun n >>= \case
          (0::Atom) -> pure False
          1         -> pure True
          _         -> fail "Atom is not a valid loobean"


-- Integer ---------------------------------------------------------------------

instance ToNoun Integer where
    toNoun = toNoun . (fromIntegral :: Integer -> Natural)

instance FromNoun Integer where
    parseNoun = named "Integer" . fmap natInt . parseNoun
      where
        natInt :: Natural -> Integer
        natInt = fromIntegral


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
instance ToNoun Word128 where toNoun = wordToNoun
instance ToNoun Word256 where toNoun = wordToNoun
instance ToNoun Word512 where toNoun = wordToNoun

instance FromNoun Word    where parseNoun = named "Word"    . nounToWord
instance FromNoun Word8   where parseNoun = named "Word8"   . nounToWord
instance FromNoun Word16  where parseNoun = named "Word16"  . nounToWord
instance FromNoun Word32  where parseNoun = named "Word32"  . nounToWord
instance FromNoun Word64  where parseNoun = named "Word64"  . nounToWord
instance FromNoun Word128 where parseNoun = named "Word128" . nounToWord
instance FromNoun Word256 where parseNoun = named "Word256" . nounToWord
instance FromNoun Word512 where parseNoun = named "Word512" . nounToWord


-- Maybe is `unit` -------------------------------------------------------------

-- TODO Consider enforcing that `a` must be a cell.
instance ToNoun a => ToNoun (Maybe a) where
  toNoun Nothing  = Atom 0
  toNoun (Just x) = Cell (Atom 0) (toNoun x)

instance FromNoun a => FromNoun (Maybe a) where
  parseNoun = named "Maybe" . \case
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
  parseNoun = named "()" . \case
    Atom 0 -> pure ()
    x      -> fail ("expecting `~`, but got " <> show x)

instance (ToNoun a, ToNoun b) => ToNoun (a, b) where
  toNoun (x, y) = Cell (toNoun x) (toNoun y)

instance (FromNoun a, FromNoun b) => FromNoun (a, b) where
  parseNoun = named ("(,)") . \case
    Atom n   -> fail ("expected a cell, but got an atom: " <> show n)
    Cell l r -> (,) <$> parseNoun l <*> parseNoun r


instance (ToNoun a, ToNoun b, ToNoun c) => ToNoun (a, b, c) where
  toNoun (x, y, z) = toNoun (x, (y, z))

instance (FromNoun a, FromNoun b, FromNoun c) => FromNoun (a, b, c) where
  parseNoun n = named "(,,)" $ do
    (x, t) <- parseNoun n
    (y, z) <- parseNoun t
    pure (x, y, z)

instance (ToNoun a, ToNoun b, ToNoun c, ToNoun d) => ToNoun (a, b, c, d) where
  toNoun (p, q, r, s) = toNoun (p, (q, r, s))

instance (FromNoun a, FromNoun b, FromNoun c, FromNoun d)
      => FromNoun (a, b, c, d)
      where
  parseNoun n = named "(,,,)" $ do
    (p, tail) <- parseNoun n
    (q, r, s) <- parseNoun tail
    pure (p, q, r, s)

instance (ToNoun a, ToNoun b, ToNoun c, ToNoun d, ToNoun e)
      => ToNoun (a, b, c, d, e) where
  toNoun (p, q, r, s, t) = toNoun (p, (q, r, s, t))

instance (FromNoun a, FromNoun b, FromNoun c, FromNoun d, FromNoun e)
      => FromNoun (a, b, c, d, e)
      where
  parseNoun n = named "(,,,,)"$ do
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
  parseNoun n = named "(,,,,,)" $ do
    (p, tail)       <- parseNoun n
    (q, r, s, t, u) <- parseNoun tail
    pure (p, q, r, s, t, u)

instance ( FromNoun a, FromNoun b, FromNoun c, FromNoun d, FromNoun e
         , FromNoun f, FromNoun g
         )
      => FromNoun (a, b, c, d, e, f, g)
      where
  parseNoun n = named "(,,,,,,)" $ do
    (p, tail)          <- parseNoun n
    (q, r, s, t, u, v) <- parseNoun tail
    pure (p, q, r, s, t, u, v)

instance ( FromNoun a, FromNoun b, FromNoun c, FromNoun d, FromNoun e
         , FromNoun f, FromNoun g, FromNoun h
         )
      => FromNoun (a, b, c, d, e, f, g, h)
      where
  parseNoun n = named "(,,,,,,,)" $ do
    (p, tail)             <- parseNoun n
    (q, r, s, t, u, v, w) <- parseNoun tail
    pure (p, q, r, s, t, u, v, w)

instance ( FromNoun a, FromNoun b, FromNoun c, FromNoun d, FromNoun e
         , FromNoun f, FromNoun g, FromNoun h, FromNoun i
         )
      => FromNoun (a, b, c, d, e, f, g, h, i)
      where
  parseNoun n = named "(,,,,,,,,)" $ do
    (p, tail)                <- parseNoun n
    (q, r, s, t, u, v, w, x) <- parseNoun tail
    pure (p, q, r, s, t, u, v, w, x)

instance ( FromNoun a, FromNoun b, FromNoun c, FromNoun d, FromNoun e
         , FromNoun f, FromNoun g, FromNoun h, FromNoun i, FromNoun j
         )
      => FromNoun (a, b, c, d, e, f, g, h, i, j)
      where
  parseNoun n = named "(,,,,,,,,,)" $ do
    (p, tail)                   <- parseNoun n
    (q, r, s, t, u, v, w, x, y) <- parseNoun tail
    pure (p, q, r, s, t, u, v, w, x, y)
