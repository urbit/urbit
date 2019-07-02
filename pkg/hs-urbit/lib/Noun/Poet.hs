{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}

module Noun.Poet where

import ClassyPrelude hiding (fromList)
import Control.Lens

import Control.Applicative
import Control.Monad
import Noun
import Noun.Atom
import Noun.Pill
import Data.Void
import Data.Word
import GHC.Natural
import GHC.Generics hiding (from)

import qualified GHC.Generics as GHC

import Data.Typeable (Typeable)
import RIO           (decodeUtf8Lenient)

import qualified Data.Char          as C
import qualified Control.Monad.Fail as Fail


-- Types For Hoon Constructs ---------------------------------------------------

{-|
    `Nullable a <-> ?@(~ a)`

    This is distinct from `unit`, since there is no tag on the non-atom
    case, therefore `a` must always be cell type.
-}
data Nullable a = Nil | NotNil a
  deriving (Eq, Ord, Show)

newtype Tour = Tour [Char]
  deriving (Eq, Ord, Show)

newtype Tape = Tape ByteString
  deriving newtype (Eq, Ord, Show, IsString)

newtype Cord = Cord { unCord :: ByteString }
  deriving newtype (Eq, Ord, Show, IsString, NFData)

-- Chars -----------------------------------------------------------------------

instance ToNoun Char where
  toNoun = toNoun . (fromIntegral :: Int -> Word32) . C.ord

instance FromNoun Char where
  parseNoun n = do
    w :: Word32 <- parseNoun n
    pure $ C.chr $ fromIntegral w

-- Pretty Printing -------------------------------------------------------------

type Tang = [Tank]

data Tank
    = TLeaf Tape
    | TPlum Plum
    | TPalm (Tape, Tape, Tape, Tape) [Tank]
    | TRose (Tape, Tape, Tape) [Tank]
  deriving (Eq, Ord, Show)

type Tile = Cord

data WideFmt
    = WideFmt { delimit :: Tile, enclose :: Maybe (Tile, Tile) }
  deriving (Eq, Ord, Show)

data TallFmt
    = TallFmt { intro   :: Tile, indef   :: Maybe (Tile, Tile) }
  deriving (Eq, Ord, Show)

data PlumFmt
    = PlumFmt (Maybe WideFmt) (Maybe TallFmt)
  deriving (Eq, Ord, Show)

data Plum
    = PAtom Cord
    | PPara Tile [Cord]
    | PTree PlumFmt [Plum]
    | PSbrk Plum
  deriving (Eq, Ord, Show)


-- IResult ---------------------------------------------------------------------

data IResult a = IError NounPath String | ISuccess a
  deriving (Eq, Show, Typeable, Functor, Foldable, Traversable)

instance Applicative IResult where
    pure  = ISuccess
    (<*>) = ap

instance Fail.MonadFail IResult where
    fail err = traceM ("!" <> err <> "!") >> IError [] err

instance Monad IResult where
    return = pure
    fail   = Fail.fail
    ISuccess a      >>= k = k a
    IError path err >>= _ = IError path err

instance MonadPlus IResult where
    mzero = fail "mzero"
    mplus a@(ISuccess _) _ = a
    mplus _ b              = b

instance Alternative IResult where
    empty = mzero
    (<|>) = mplus

instance Semigroup (IResult a) where
    (<>) = mplus

instance Monoid (IResult a) where
    mempty  = fail "mempty"
    mappend = (<>)


-- Result ----------------------------------------------------------------------

data Result a = Error String | Success a
  deriving (Eq, Show, Typeable, Functor, Foldable, Traversable)

instance Applicative Result where
    pure  = Success
    (<*>) = ap

instance Fail.MonadFail Result where
    fail err = Error err

instance Monad Result where
    return = pure
    fail   = Fail.fail

    Success a >>= k = k a
    Error err >>= _ = Error err

instance MonadPlus Result where
    mzero = fail "mzero"
    mplus a@(Success _) _ = a
    mplus _ b             = b

instance Alternative Result where
    empty = mzero
    (<|>) = mplus

instance Semigroup (Result a) where
    (<>) = mplus
    {-# INLINE (<>) #-}

instance Monoid (Result a) where
    mempty  = fail "mempty"
    mappend = (<>)


-- "Parser" --------------------------------------------------------------------

type Failure f r   = NounPath -> String -> f r
type Success a f r = a -> f r

newtype Parser a = Parser {
  runParser :: forall f r.  NounPath -> Failure f r -> Success a f r -> f r
}

instance Monad Parser where
    m >>= g = Parser $ \path kf ks -> let ks' a = runParser (g a) path kf ks
                                       in runParser m path kf ks'
    return = pure
    fail = Fail.fail

instance Fail.MonadFail Parser where
    fail msg = Parser $ \path kf _ks -> kf (reverse path) msg

instance Functor Parser where
    fmap f m = Parser $ \path kf ks -> let ks' a = ks (f a)
                                        in runParser m path kf ks'

apP :: Parser (a -> b) -> Parser a -> Parser b
apP d e = do
  b <- d
  b <$> e

instance Applicative Parser where
    pure a = Parser $ \_path _kf ks -> ks a
    (<*>) = apP

instance Alternative Parser where
    empty = fail "empty"
    (<|>) = mplus

instance MonadPlus Parser where
    mzero = fail "mzero"
    mplus a b = Parser $ \path kf ks -> let kf' _ _ = runParser b path kf ks
                                         in runParser a path kf' ks

instance Semigroup (Parser a) where
    (<>) = mplus

instance Monoid (Parser a) where
    mempty  = fail "mempty"
    mappend = (<>)


-- Conversion ------------------------------------------------------------------

class FromNoun a where
  parseNoun :: Noun -> Parser a

class ToNoun a where
  toNoun :: a -> Noun

  default toNoun :: (Generic a, GToNoun (Rep a)) => a -> Noun
  toNoun = genericToNoun


-- Generic Deriving ToNoun -----------------------------------------------------

-- TODO Handle enums

class GToNoun f where
  gToNoun :: f a -> Noun

genericToNoun :: (Generic a, GToNoun (Rep a)) => a -> Noun
genericToNoun = gToNoun . GHC.from

--------------------------------------------------------------------------------

instance GToNoun V1 where gToNoun _  = undefined
instance GToNoun U1 where gToNoun U1 = Atom 0

instance ToNoun a => GToNoun (K1 i a) where
    gToNoun = toNoun . unK1

instance (GToNoun a, GToNoun b) => GToNoun (a :*: b) where
    gToNoun (x :*: y) = Cell (gToNoun x) (gToNoun y)

instance (GToNoun a, GToNoun b) => GToNoun (a :+: b) where
    gToNoun (L1 x) = gToNoun x
    gToNoun (R1 x) = gToNoun x

instance GToNoun a => GToNoun (S1 c a) where
    gToNoun x = gToNoun (unM1 x)

instance GToNoun a => GToNoun (D1 c a) where
    gToNoun x = gToNoun (unM1 x)

instance (GToNoun f, Constructor c) => GToNoun (C1 c f) where
    gToNoun x = Cell tag val
                  where tag = toNoun (hsToHoon $ conName x)
                        val = gToNoun (unM1 x)

--------------------------------------------------------------------------------

hsToHoon :: String -> Text
hsToHoon = go []
  where
    go acc []     = pack $ intercalate "-" $ reverse acc
    go acc (c:cs) = go (elem:acc) remain
      where
        head           = C.toLower c
        (tail, remain) = break C.isUpper cs
        elem           = head:tail

-- Copy-Pasta ------------------------------------------------------------------

class HasConstructor (f :: * -> *) where
  gConsName :: f x -> String

instance HasConstructor f => HasConstructor (D1 c f) where
  gConsName (M1 x) = gConsName x

instance (HasConstructor x, HasConstructor y) => HasConstructor (x :+: y) where
  gConsName (L1 l) = gConsName l
  gConsName (R1 r) = gConsName r

instance Constructor c => HasConstructor (C1 c f) where
  gConsName x = conName x

consName :: (HasConstructor (Rep a), Generic a) => a -> String
consName = gConsName . GHC.from

--------------------------------------------------------------------------------

int2Word :: Int -> Word
int2Word = fromIntegral

word2Int :: Word -> Int
word2Int = fromIntegral

instance ToNoun ByteString where
  toNoun bs = toNoun (int2Word (length bs), bs ^. from (pill . pillBS))

instance ToNoun Text where -- XX TODO
  toNoun t = toNoun (Cord (encodeUtf8 t))

instance FromNoun Text where -- XX TODO
  parseNoun n = do
    Cord c <- parseNoun n
    pure (decodeUtf8Lenient c)

instance FromNoun ByteString where
  parseNoun x = do
    (word2Int -> len, atom) <- parseNoun x
    let bs = atom ^. pill . pillBS
    pure $ case compare (length bs) len of
      EQ -> bs
      LT -> bs <> replicate (len - length bs) 0
      GT -> take len bs

--------------------------------------------------------------------------------

newtype Term = MkTerm Text
  deriving newtype (Eq, Ord, Show)

instance ToNoun Term where -- XX TODO
  toNoun (MkTerm t) = toNoun (Cord (encodeUtf8 t))

instance FromNoun Term where -- XX TODO
  parseNoun n = do
    Cord c <- parseNoun n
    pure (MkTerm (decodeUtf8Lenient c))

--------------------------------------------------------------------------------

newtype Knot = MkKnot Text
  deriving newtype (Eq, Ord, Show)

instance ToNoun Knot where -- XX TODO
  toNoun (MkKnot t) = toNoun (Cord (encodeUtf8 t))

instance FromNoun Knot where -- XX TODO
  parseNoun n = do
    Cord c <- parseNoun n
    pure (MkKnot (decodeUtf8Lenient c))

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

fromNoun :: FromNoun a => Noun -> Maybe a
fromNoun n = runParser (parseNoun n) [] onFail onSuccess
  where
    onFail p m  = Nothing
    onSuccess x = Just x

fromNounErr :: FromNoun a => Noun -> Either Text a
fromNounErr n = runParser (parseNoun n) [] onFail onSuccess
  where
    onFail p m  = Left (pack m)
    onSuccess x = Right x

_Poet :: (ToNoun a, FromNoun a) => Prism' Noun a
_Poet = prism' toNoun fromNoun


-- Trivial Conversion ----------------------------------------------------------

instance ToNoun Void where
  toNoun = absurd

instance FromNoun Void where
  parseNoun = fail "Can't produce void"

instance ToNoun Noun where
  toNoun = id

instance FromNoun Noun where
  parseNoun = pure


-- Loobean Conversion ----------------------------------------------------------

instance ToNoun Bool where
  toNoun True  = Atom 0
  toNoun False = Atom 1

instance FromNoun Bool where
  parseNoun (Atom 0)   = pure True
  parseNoun (Atom 1)   = pure False
  parseNoun (Cell _ _) = fail "expecting a bool, but got a cell"
  parseNoun (Atom a)   = fail ("expecting a bool, but got " <> show a)


-- Atom Conversion -------------------------------------------------------------

instance ToNoun Atom where
  toNoun = Atom

instance FromNoun Atom where
  parseNoun (Cell _ _) = fail "Expecting an atom, but got a cell"
  parseNoun (Atom a)   = pure a


-- Natural Conversion-----------------------------------------------------------

instance ToNoun Natural   where toNoun    = toNoun . MkAtom
instance FromNoun Natural where parseNoun = fmap unAtom . parseNoun

instance ToNoun Integer   where
    toNoun = toNoun . (fromIntegral :: Integer -> Natural)

instance FromNoun Integer where
    parseNoun = fmap ((fromIntegral :: Natural -> Integer) . unAtom) . parseNoun


-- Word Conversion -------------------------------------------------------------

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


-- Nullable Conversion ---------------------------------------------------------

-- TODO Consider enforcing that `a` must be a cell.
instance ToNoun a => ToNoun (Nullable a) where
  toNoun Nil        = Atom 0
  toNoun (NotNil x) = toNoun x

instance FromNoun a => FromNoun (Nullable a) where
  parseNoun (Atom 0) = pure Nil
  parseNoun (Atom n) = fail ("Nullable: expected ?@(~ ^), but got " <> show n)
  parseNoun n        = NotNil <$> parseNoun n


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


-- List Conversion -------------------------------------------------------------

instance ToNoun a => ToNoun [a] where
  toNoun xs = fromList (toNoun <$> xs)

instance FromNoun a => FromNoun [a] where
  parseNoun (Atom 0)   = pure []
  parseNoun (Atom _)   = fail "list terminated with non-null atom"
  parseNoun (Cell l r) = (:) <$> parseNoun l <*> parseNoun r


-- Cord Conversion -------------------------------------------------------------

instance ToNoun Cord where
  toNoun (Cord bs) = Atom (bs ^. from (pill . pillBS))

instance FromNoun Cord where
  parseNoun n = do
    atom <- parseNoun n
    pure $ Cord (atom ^. pill . pillBS)


-- Tank and Plum Conversion ----------------------------------------------------

instance ToNoun WideFmt where toNoun (WideFmt x xs)      = toNoun (x, xs)
instance ToNoun TallFmt where toNoun (TallFmt x xs)      = toNoun (x, xs)
instance ToNoun PlumFmt where toNoun (PlumFmt wide tall) = toNoun (wide, tall)

instance FromNoun WideFmt where parseNoun = fmap (uncurry WideFmt) . parseNoun
instance FromNoun TallFmt where parseNoun = fmap (uncurry TallFmt) . parseNoun
instance FromNoun PlumFmt where parseNoun = fmap (uncurry PlumFmt) . parseNoun

instance ToNoun Plum where
  toNoun = \case
    PAtom cord -> toNoun cord
    PPara t cs -> toNoun (Cord "para", t, cs)
    PTree f ps -> toNoun (Cord "tree", f, ps)
    PSbrk p    -> toNoun (Cord "sbrk", p)

instance FromNoun Plum where
  parseNoun = undefined

instance ToNoun Tank where
  toNoun = pure (Atom 0)

instance FromNoun Tank where
  parseNoun _ = pure (TLeaf (Tape "TODO: Tank Parsing"))


-- Pair Conversion -------------------------------------------------------------

instance ToNoun () where
  toNoun () = Atom 0

instance FromNoun () where
  parseNoun (Atom 0) = pure ()
  parseNoun x        = fail ("expecting `~`, but got " <> showNoun x)

instance (ToNoun a, ToNoun b) => ToNoun (a, b) where
  toNoun (x, y) = Cell (toNoun x) (toNoun y)

instance (FromNoun a, FromNoun b) => FromNoun (a, b) where
  parseNoun (Atom n)   = fail ("expected a cell, but got an atom: " <> show n)
  parseNoun (Cell l r) = (,) <$> parseNoun l <*> parseNoun r


-- Trel Conversion -------------------------------------------------------------

instance (ToNoun a, ToNoun b, ToNoun c) => ToNoun (a, b, c) where
  toNoun (x, y, z) = toNoun (x, (y, z))

instance (FromNoun a, FromNoun b, FromNoun c) => FromNoun (a, b, c) where
  parseNoun n = do
    (x, t) <- parseNoun n
    (y, z) <- parseNoun t
    pure (x, y, z)


-- Quad Conversion -------------------------------------------------------------

instance (ToNoun a, ToNoun b, ToNoun c, ToNoun d) => ToNoun (a, b, c, d) where
  toNoun (p, q, r, s) = toNoun (p, (q, r, s))

instance (FromNoun a, FromNoun b, FromNoun c, FromNoun d)
      => FromNoun (a, b, c, d)
      where
  parseNoun n = do
    (p, tail) <- parseNoun n
    (q, r, s) <- parseNoun tail
    pure (p, q, r, s)


-- Pent Conversion ------------------------------------------------------------

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


-- Sext Conversion ------------------------------------------------------------

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


-- This Shouldn't Be Here ------------------------------------------------------

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

showNoun :: Noun -> String
showNoun = \case
    Atom a   -> showAtom a
    Cell x y -> fmtCell (showNoun <$> (x : toTuple y))
  where
    fmtCell :: [String] -> String
    fmtCell xs = "[" <> intercalate " " xs <> "]"

pPrintAtom :: Atom -> IO ()
pPrintAtom = putStrLn . pack . showAtom

pPrintNoun :: Noun -> IO ()
pPrintNoun = putStrLn . pack . showNoun
