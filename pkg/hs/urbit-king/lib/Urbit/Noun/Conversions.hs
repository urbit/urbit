{-# OPTIONS_GHC -Wwarn #-}

{-|
    Large Library of conversion between various types and Nouns.
-}

module Urbit.Noun.Conversions
  ( Nullable(..), Jammed(..), AtomCell(..)
  , Word128, Word256, Word512
  , Bytes(..), Octs(..), File(..)
  , Cord(..), Knot(..), Term(..), Tape(..), Tour(..)
  , BigTape(..), BigCord(..)
  , Wall, Each(..)
  , UD(..), UV(..), UW(..), cordToUW
  , Mug(..), Path(..), EvilPath(..), Ship(..)
  , Lenient(..), pathToFilePath, filePathToPath
  ) where

import ClassyPrelude hiding (hash)

import Control.Lens         hiding (Each, Index, (<.>))
import Data.Void
import Data.Word
import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()
import Urbit.Atom
import Urbit.Noun.Convert
import Urbit.Noun.Core
import Urbit.Noun.TH

import Data.LargeWord   (LargeKey, Word128, Word256)
import GHC.Exts         (chr#, isTrue#, leWord#, word2Int#)
import GHC.Natural      (Natural)
import GHC.Types        (Char(C#))
import GHC.Word         (Word32(W32#))
import Prelude          ((!!))
import RIO              (decodeUtf8Lenient)
import RIO.FilePath     (joinPath, splitDirectories, takeBaseName,
                         takeDirectory, takeExtension, (<.>), (</>))
import System.IO.Unsafe (unsafePerformIO)
import Text.Show.Pretty (ppShow)
import Urbit.Noun.Cue   (cue)
import Urbit.Noun.Jam   (jam)

import qualified Data.Char                as C
import qualified Data.Text.Encoding       as T
import qualified Data.Text.Encoding.Error as T


-- Noun ------------------------------------------------------------------------

instance ToNoun Noun where
  toNoun = id

instance FromNoun Noun where
  parseNoun = pure


--- Atom -----------------------------------------------------------------------

instance ToNoun Atom where
  toNoun = Atom

instance FromNoun Atom where
  parseNoun = named "Atom" . \case
    Atom a   -> pure a
    Cell _ _ -> fail "Expecting an atom, but got a cell"


-- Void ------------------------------------------------------------------------

instance ToNoun Void where
  toNoun = absurd

instance FromNoun Void where
  parseNoun _ = named "Void" $ fail "Can't produce void"


-- Cord ------------------------------------------------------------------------

newtype Cord = Cord { unCord :: Text }
  deriving newtype (Eq, Ord, Show, IsString, NFData)

instance ToNoun Cord where
  toNoun = textToUtf8Atom . unCord

instance FromNoun Cord where
  parseNoun = named "Cord" . fmap Cord . parseNounUtf8Atom


-- Decimal Cords ---------------------------------------------------------------

newtype UD = UD { unUD :: Word }
  deriving newtype (Eq, Ord, Show, Enum, Real, Integral, Num)

instance ToNoun UD where
  toNoun = toNoun . Cord . tshow . unUD

instance FromNoun UD where
  parseNoun n = named "UD" do
    Cord t <- parseNoun n
    readMay t & \case
      Nothing -> fail ("invalid decimal atom: " <> unpack (filter (/= '.') t))
      Just vl -> pure (UD vl)


--------------------------------------------------------------------------------

uTypeAddDots :: String -> String
uTypeAddDots = reverse . go . reverse
  where
    go s = if null tel then hed
                       else hed <> "." <> go tel
      where
        hed = take 5 s
        tel = drop 5 s

convertToU :: [Char] -> [Char] -> Atom -> String
convertToU baseMap prefix = go []
  where
    go acc 0 = "0" <> prefix <> uTypeAddDots acc
    go acc n = go (char n : acc) (n `div` len)

    char n = baseMap !! (fromIntegral (n `mod` len))

    len = fromIntegral (length baseMap)

convertFromU :: (Char -> Maybe Atom) -> Char -> Atom -> String -> Maybe Atom
convertFromU fetch prefix length = \case
    ('0':prefix:cs) -> go (0, 0) (reverse cs)
    _               -> Nothing
  where
    go (i, acc) []         = pure acc
    go (i, acc) ('.' : cs) = go (i, acc) cs
    go (i, acc) (c   : cs) = do
        n <- fetch c
        go (i+1, acc+(length^i)*n) cs


-- @uv
newtype UV = UV { unUV :: Atom }
  deriving newtype (Eq, Ord, Show, Num, Enum, Real, Integral)

instance ToNoun UV where
    toNoun = toNoun . Cord . pack . toUV . fromIntegral . unUV

instance FromNoun UV where
    parseNoun n = do
      Cord c <- parseNoun n
      case fromUV $ unpack c of
        Nothing -> fail ("Invalid @uv: " <> unpack c)
        Just uv -> pure (UV uv)

fromUV :: String -> Maybe Atom
fromUV = convertFromU uvCharNum 'v' (fromIntegral $ length base32Chars)

toUV :: Atom -> String
toUV = convertToU base32Chars "v"

base32Chars :: [Char]
base32Chars = (['0'..'9'] <> ['a'..'v'])

uvCharNum :: Char -> Maybe Atom
uvCharNum = \case
  '0' -> pure 0
  '1' -> pure 1
  '2' -> pure 2
  '3' -> pure 3
  '4' -> pure 4
  '5' -> pure 5
  '6' -> pure 6
  '7' -> pure 7
  '8' -> pure 8
  '9' -> pure 9
  'a' -> pure 10
  'b' -> pure 11
  'c' -> pure 12
  'd' -> pure 13
  'e' -> pure 14
  'f' -> pure 15
  'g' -> pure 16
  'h' -> pure 17
  'i' -> pure 18
  'j' -> pure 19
  'k' -> pure 20
  'l' -> pure 21
  'm' -> pure 22
  'n' -> pure 23
  'o' -> pure 24
  'p' -> pure 25
  'q' -> pure 26
  'r' -> pure 27
  's' -> pure 28
  't' -> pure 29
  'u' -> pure 30
  'v' -> pure 31
  _   -> Nothing

--------------------------------------------------------------------------------

-- @uw
newtype UW = UW { unUW :: Atom }
  deriving newtype (Eq, Ord, Show, Num, Enum, Real, Integral)

instance ToNoun UW where
  toNoun = toNoun . Cord . pack . toUW . fromIntegral . unUW

instance FromNoun UW where
  parseNoun n = do
    Cord c <- parseNoun n
    case fromUW $ unpack c of
      Nothing -> fail ("Invalid @uw: " <> unpack c)
      Just uw -> pure (UW uw)

fromUW :: String -> Maybe Atom
fromUW = convertFromU uwCharNum 'w' (fromIntegral $ length base64Chars)

toUW :: Atom -> String
toUW = convertToU base64Chars "w"

base64Chars :: [Char]
base64Chars = (['0'..'9'] <> ['a'..'z'] <> ['A'..'Z'] <> ['-', '~'])

uwCharNum :: Char -> Maybe Atom
uwCharNum = \case
  '0' -> pure 0
  '1' -> pure 1
  '2' -> pure 2
  '3' -> pure 3
  '4' -> pure 4
  '5' -> pure 5
  '6' -> pure 6
  '7' -> pure 7
  '8' -> pure 8
  '9' -> pure 9
  'a' -> pure 10
  'b' -> pure 11
  'c' -> pure 12
  'd' -> pure 13
  'e' -> pure 14
  'f' -> pure 15
  'g' -> pure 16
  'h' -> pure 17
  'i' -> pure 18
  'j' -> pure 19
  'k' -> pure 20
  'l' -> pure 21
  'm' -> pure 22
  'n' -> pure 23
  'o' -> pure 24
  'p' -> pure 25
  'q' -> pure 26
  'r' -> pure 27
  's' -> pure 28
  't' -> pure 29
  'u' -> pure 30
  'v' -> pure 31
  'w' -> pure 32
  'x' -> pure 33
  'y' -> pure 34
  'z' -> pure 35
  'A' -> pure 36
  'B' -> pure 37
  'C' -> pure 38
  'D' -> pure 39
  'E' -> pure 40
  'F' -> pure 41
  'G' -> pure 42
  'H' -> pure 43
  'I' -> pure 44
  'J' -> pure 45
  'K' -> pure 46
  'L' -> pure 47
  'M' -> pure 48
  'N' -> pure 49
  'O' -> pure 50
  'P' -> pure 51
  'Q' -> pure 52
  'R' -> pure 53
  'S' -> pure 54
  'T' -> pure 55
  'U' -> pure 56
  'V' -> pure 57
  'W' -> pure 58
  'X' -> pure 59
  'Y' -> pure 60
  'Z' -> pure 61
  '-' -> pure 62
  '~' -> pure 63
  _   -> Nothing

-- Maybe parses the underlying atom value from a text printed in UW format.
cordToUW :: Cord -> Maybe UW
cordToUW = fromNoun . toNoun

-- Char ------------------------------------------------------------------------

instance ToNoun Char where
  toNoun = Atom . fromIntegral . C.ord

{-
    Hack: pulled this logic from Data.Char impl.
-}
instance FromNoun Char where
  parseNoun n = named "Char" $ do
    W32# w :: Word32 <- parseNoun n
    if isTrue# (w `leWord#` 0x10FFFF##)
      then pure (C# (chr# (word2Int# w)))
      else fail "Word is not a valid character."


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


-- Lenient ---------------------------------------------------------------------

data Lenient a
    = FailParse Noun
    | GoodParse a
  deriving (Eq, Ord, Show)

instance FromNoun a => FromNoun (Lenient a) where
  parseNoun n =
      (GoodParse <$> parseNoun n) <|> fallback
    where
      fallback =
        fromNounErr n & \case
          Right x  -> pure (GoodParse x)
          Left err -> do
            traceM ("LENIENT.FromNoun: " <> show err)
            traceM (ppShow n)
            pure (FailParse n)

instance ToNoun a => ToNoun (Lenient a) where
  toNoun (FailParse n) = trace ("LENIENT.ToNoun: " <> show n)
                           n
  toNoun (GoodParse x) = toNoun x


-- Todo -- Debugging Hack ------------------------------------------------------

newtype Todo a = Todo a
  deriving newtype (Eq, Ord, ToNoun)

instance Show (Todo a) where
  show (Todo _) = "TODO"

instance FromNoun a => FromNoun (Todo a) where
  parseNoun n = do
      fromNounErr n & \case
        Right x -> pure (Todo x)
        Left er -> do
          traceM ("[TODO]: " <> show er <> "\n" <> ppShow n <> "\n")
          fail (show er)


-- Nullable --------------------------------------------------------------------

{-|
    `Nullable a <-> ?@(~ a)`

    This is distinct from `unit`, since there is no tag on the non-atom
    case, therefore `a` must always be cell type.
-}
data Nullable a = None | Some a
  deriving (Eq, Ord, Show)

instance ToNoun a => ToNoun (Nullable a) where
  toNoun = toNoun . \case None   -> ACAtom ()
                          Some x -> ACCell x

instance FromNoun a => FromNoun (Nullable a) where
  parseNoun n = named "Nullable" $ do
    parseNoun n >>= \case
      (ACAtom ()) -> pure None
      (ACCell x)  -> pure (Some x)


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

{-
    A `tape` is a list of utf8 bytes.
-}
newtype Tape = Tape { unTape :: Text }
  deriving newtype (Eq, Ord, Show, Semigroup, Monoid, IsString)

instance ToNoun Tape where
  toNoun = toNoun . (unpack :: ByteString -> [Word8]) . encodeUtf8 . unTape

instance FromNoun Tape where
  parseNoun n = named "Tape" $ do
    as :: [Word8] <- parseNoun n
    T.decodeUtf8' (pack as) & \case
        Left err -> fail (show err)
        Right tx -> pure (Tape tx)


-- Wall -- Text Lines ----------------------------------------------------------

type Wall = [Tape]


-- Big Cord -- Don't Print -----------------------------------------------------

newtype BigCord = BigCord Cord
  deriving newtype (Eq, Ord, ToNoun, FromNoun, IsString)

instance Show BigCord where
  show (BigCord (Cord t)) = show (take 32 t <> "...")


-- Big Tape -- Don't Print -----------------------------------------------------

newtype BigTape = BigTape Tape
  deriving newtype (Eq, Ord, ToNoun, FromNoun, IsString)

instance Show BigTape where
  show (BigTape (Tape t)) = show (take 32 t <> "...")


-- Bytes -----------------------------------------------------------------------

newtype Bytes = MkBytes { unBytes :: ByteString }
  deriving newtype (Eq, Ord, Show)

instance ToNoun Bytes where
    toNoun = Atom . bytesAtom . unBytes

instance FromNoun Bytes where
    parseNoun = named "Bytes" . fmap (MkBytes . atomBytes) . parseNoun


-- Octs ------------------------------------------------------------------------

newtype Octs = Octs { unOcts :: ByteString }
  deriving newtype (Eq, Ord, Show, IsString)

instance ToNoun Octs where
  toNoun (Octs bs) =
      toNoun (int2Word (length bs), bytesAtom bs)
    where
      int2Word :: Int -> Word
      int2Word = fromIntegral

instance FromNoun Octs where
    parseNoun x = named "Octs" $ do
        (word2Int -> len, atom) <- parseNoun x
        let bs = atomBytes atom
        pure $ Octs $ case compare (length bs) len of
          EQ -> bs
          LT -> bs <> replicate (len - length bs) 0
          GT -> take len bs
      where
        word2Int :: Word -> Int
        word2Int = fromIntegral


-- File Contents -- Don't Print ------------------------------------------------

newtype File = File { unFile :: Octs }
  deriving newtype (Eq, Ord, IsString, ToNoun, FromNoun)

instance Show File where
  show (File (Octs bs)) = show (take 32 bs <> "...")


-- Knot ------------------------------------------------------------------------

{-
    Knot (@ta) is an array of Word8 encoding an ASCII string.
-}
newtype Knot = MkKnot { unKnot :: Text }
  deriving newtype (Eq, Ord, Show, Semigroup, Monoid, IsString)

instance ToNoun Knot where
  toNoun = textToUtf8Atom . unKnot

instance FromNoun Knot where
  parseNoun n = named "Knot" $ do
    txt <- parseNounUtf8Atom n
    if all C.isAscii txt
      then pure (MkKnot txt)
      else fail ("Non-ASCII chars in knot: " <> unpack txt)


-- Term ------------------------------------------------------------------------

{-
    A Term (@tas) is a Knot satisfying the regular expression:

        ([a-z][a-z0-9]*(-[a-z0-9]+)*)?
-}
newtype Term = MkTerm { unTerm :: Text }
  deriving newtype (Eq, Ord, Show, Semigroup, Monoid, IsString)

instance ToNoun Term where -- XX TODO
  toNoun = textToUtf8Atom . unTerm

knotRegex :: Text
knotRegex = "([a-z][a-z0-9]*(-[a-z0-9]+)*)?"

instance FromNoun Term where -- XX TODO
  parseNoun n = named "Term" $ do
    MkKnot t <- parseNoun n
    if t =~ knotRegex
      then pure (MkTerm t)
      else fail ("Term not valid symbol: " <> unpack t)


-- Ship ------------------------------------------------------------------------

newtype Ship = Ship Word128 -- @p
  deriving newtype (Eq, Ord, Show, Enum, Real, Integral, Num, ToNoun, FromNoun)


-- Path ------------------------------------------------------------------------

newtype Path = Path { unPath :: [Knot] }
  deriving newtype (Eq, Ord, Semigroup, Monoid)

instance Show Path where
  show = show . intercalate "/" . ("":) . unPath

newtype EvilPath = EvilPath { unEvilPath :: [Atom] }
  deriving newtype (Eq, Ord, Semigroup, Monoid)

instance Show EvilPath where
  show = show . unEvilPath

pathToFilePath :: Path -> FilePath
pathToFilePath p = joinPath components
  where
    elements :: [String] = map (unpack . unKnot) (unPath p)
    components = case reverse elements of
      [] -> []
      [p] -> [p]
      (ext : fname : dirs) -> (reverse dirs) <> [(fname <.> ext)]

-- Takes a filepath and converts it to a clay path, changing the '.' to a '/'
-- and removing any prefixed '/'.
filePathToPath :: FilePath -> Path
filePathToPath fp = Path path
  where
    path = map (MkKnot . pack) (dir ++ file)
    dir = case (splitDirectories $ (takeDirectory fp)) of
      ["."]    -> []
      ("/":xs) -> xs
      x        -> x
    file = if ext /= "" then [takeBaseName fp, ext] else [takeBaseName fp]
    ext = case takeExtension fp of
      ('.':xs) -> xs
      x        -> x

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
          (0::Atom) -> pure True
          1         -> pure False
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

-- Each is a direct translation of Hoon +each, preserving order
data Each a b
    = EachYes a
    | EachNo b
  deriving (Eq, Ord, Show)

instance (ToNoun a, ToNoun b) => ToNoun (Each a b) where
    toNoun (EachYes x) = C (A 0) (toNoun x)
    toNoun (EachNo x)  = C (A 1) (toNoun x)

instance (FromNoun a, FromNoun b) => FromNoun (Each a b) where
    parseNoun n = named "Each" $ do
        (Atom tag, v) <- parseNoun n
        case tag of
            0 -> named "&" (EachYes <$> parseNoun v)
            1 -> named "|" (EachNo <$> parseNoun v)
            n -> fail ("Each has invalid head-atom: " <> show n)

-- Tuple Conversions -----------------------------------------------------------

instance ToNoun () where
    toNoun () = Atom 0

instance FromNoun () where
    parseNoun = named "()" . \case
        Atom 0 -> pure ()
        x      -> fail ("expecting `~`, but got " <> show x)

instance (ToNoun a, ToNoun b) => ToNoun (a, b) where
    toNoun (x, y) = Cell (toNoun x) (toNoun y)


shortRec :: Word -> Parser a
shortRec 0 = fail "expected a record, but got an atom"
shortRec 1 = fail ("record too short, only one cell")
shortRec n = fail ("record too short, only " <> show n <> " cells")

instance (FromNoun a, FromNoun b) => FromNoun (a, b) where
  parseNoun n = named ("(,)") $ do
    case n of
      A _   -> shortRec 0
      C x y -> do
        (,) <$> named "1" (parseNoun x)
            <*> named "2" (parseNoun y)

instance (ToNoun a, ToNoun b, ToNoun c) => ToNoun (a, b, c) where
  toNoun (x, y, z) = toNoun (x, (y, z))

instance (FromNoun a, FromNoun b, FromNoun c) => FromNoun (a, b, c) where
  parseNoun n = named "(,,)" $ do
    case n of
      A _         -> shortRec 0
      C x (A _)   -> shortRec 1
      C x (C y z) ->
        (,,) <$> named "1" (parseNoun x)
             <*> named "2" (parseNoun y)
             <*> named "3" (parseNoun z)

instance (ToNoun a, ToNoun b, ToNoun c, ToNoun d) => ToNoun (a, b, c, d) where
  toNoun (p, q, r, s) = toNoun (p, (q, r, s))

instance (FromNoun a, FromNoun b, FromNoun c, FromNoun d)
      => FromNoun (a, b, c, d)
      where
  parseNoun n = named "(,,,)" $ do
    case n of
      A _               -> shortRec 0
      C _ (A _)         -> shortRec 1
      C _ (C _ (A _))   -> shortRec 2
      C p (C q (C r s)) ->
        (,,,) <$> named "1" (parseNoun p)
              <*> named "2" (parseNoun q)
              <*> named "3" (parseNoun r)
              <*> named "4" (parseNoun s)

instance (ToNoun a, ToNoun b, ToNoun c, ToNoun d, ToNoun e)
      => ToNoun (a, b, c, d, e) where
  toNoun (p, q, r, s, t) = toNoun (p, (q, r, s, t))

instance (FromNoun a, FromNoun b, FromNoun c, FromNoun d, FromNoun e)
      => FromNoun (a, b, c, d, e)
      where
  parseNoun n = named "(,,,,)" $ do
    case n of
      A _                     -> shortRec 0
      C _ (A _)               -> shortRec 1
      C _ (C _ (A _))         -> shortRec 2
      C _ (C _ (C _ (A _)))   -> shortRec 3
      C p (C q (C r (C s t))) ->
        (,,,,) <$> named "1" (parseNoun p)
               <*> named "2" (parseNoun q)
               <*> named "3" (parseNoun r)
               <*> named "4" (parseNoun s)
               <*> named "5" (parseNoun t)

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

instance (ToNoun a, ToNoun b, ToNoun c, ToNoun d, ToNoun e, ToNoun f, ToNoun g)
      => ToNoun (a, b, c, d, e, f, g) where
  toNoun (p, q, r, s, t, u, v) = toNoun (p, (q, r, s, t, u, v))

instance ( FromNoun a, FromNoun b, FromNoun c, FromNoun d, FromNoun e
         , FromNoun f, FromNoun g
         )
      => FromNoun (a, b, c, d, e, f, g)
      where
  parseNoun n = named "(,,,,,,)" $ do
    (p, tail)          <- parseNoun n
    (q, r, s, t, u, v) <- parseNoun tail
    pure (p, q, r, s, t, u, v)

instance ( ToNoun a, ToNoun b, ToNoun c, ToNoun d, ToNoun e, ToNoun f, ToNoun g
         , ToNoun h
         )
      => ToNoun (a, b, c, d, e, f, g, h) where
  toNoun (p, q, r, s, t, u, v, w) = toNoun (p, (q, r, s, t, u, v, w))

instance ( FromNoun a, FromNoun b, FromNoun c, FromNoun d, FromNoun e
         , FromNoun f, FromNoun g, FromNoun h
         )
      => FromNoun (a, b, c, d, e, f, g, h)
      where
  parseNoun n = named "(,,,,,,,)" $ do
    (p, tail)             <- parseNoun n
    (q, r, s, t, u, v, w) <- parseNoun tail
    pure (p, q, r, s, t, u, v, w)

instance ( ToNoun a, ToNoun b, ToNoun c, ToNoun d, ToNoun e, ToNoun f, ToNoun g
         , ToNoun h, ToNoun i
         )
      => ToNoun (a, b, c, d, e, f, g, h, i) where
  toNoun (p, q, r, s, t, u, v, w, x) = toNoun (p, (q, r, s, t, u, v, w, x))

instance ( FromNoun a, FromNoun b, FromNoun c, FromNoun d, FromNoun e
         , FromNoun f, FromNoun g, FromNoun h, FromNoun i
         )
      => FromNoun (a, b, c, d, e, f, g, h, i)
      where
  parseNoun n = named "(,,,,,,,,)" $ do
    (p, tail)                <- parseNoun n
    (q, r, s, t, u, v, w, x) <- parseNoun tail
    pure (p, q, r, s, t, u, v, w, x)

instance ( ToNoun a, ToNoun b, ToNoun c, ToNoun d, ToNoun e, ToNoun f, ToNoun g
         , ToNoun h, ToNoun i, ToNoun j
         )
      => ToNoun (a, b, c, d, e, f, g, h, i, j) where
  toNoun (p, q, r, s, t, u, v, w, x, y) =
    toNoun (p, (q, r, s, t, u, v, w, x, y))

instance ( FromNoun a, FromNoun b, FromNoun c, FromNoun d, FromNoun e
         , FromNoun f, FromNoun g, FromNoun h, FromNoun i, FromNoun j
         )
      => FromNoun (a, b, c, d, e, f, g, h, i, j)
      where
  parseNoun n = named "(,,,,,,,,,)" $ do
    (p, tail)                   <- parseNoun n
    (q, r, s, t, u, v, w, x, y) <- parseNoun tail
    pure (p, q, r, s, t, u, v, w, x, y)


-- Ugg -------------------------------------------------------------------------

deriveNoun ''Path
deriveNoun ''EvilPath
