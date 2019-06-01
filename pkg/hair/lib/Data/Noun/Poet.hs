module Data.Noun.Poet where

import Prelude
import Control.Lens

import Control.Applicative
import Control.Monad
import Data.Noun
import Data.Noun.Atom
import Data.Void
import GHC.Natural

import Data.List      (intercalate)
import Data.Typeable  (Typeable)
import Data.Word      (Word, Word32, Word64)

import qualified Control.Monad.Fail as Fail


-- IResult ---------------------------------------------------------------------

data IResult a = IError NounPath String | ISuccess a
  deriving (Eq, Show, Typeable, Functor, Foldable, Traversable)

instance Applicative IResult where
    pure  = ISuccess
    (<*>) = ap

instance Fail.MonadFail IResult where
    fail err = IError [] err

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

fromNoun :: FromNoun a => Noun -> Maybe a
fromNoun n = runParser (parseNoun n) [] onFail onSuccess
  where
    onFail p m  = Nothing
    onSuccess x = Just x

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

-- Bool Conversion -------------------------------------------------------------

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

-- Word Conversion -------------------------------------------------------------

instance ToNoun Word where
  toNoun = Atom . fromIntegral

instance ToNoun Word32 where
  toNoun = Atom . fromIntegral

instance FromNoun Word32 where
  parseNoun (Cell _ _) = fail "cell is not an atom"
  parseNoun (Atom a)   = pure (fromIntegral a) -- TODO Overflow

instance ToNoun Word64 where
  toNoun = Atom . fromIntegral

instance FromNoun Word64 where
  parseNoun (Cell _ _) = fail "cell is not an atom"
  parseNoun (Atom a)   = pure (fromIntegral a) -- TODO Overflow

instance ToNoun Natural where
  toNoun = toNoun . toAtom


-- Cell Conversion -------------------------------------------------------------

instance (ToNoun a, ToNoun b) => ToNoun (a, b) where
  toNoun (x, y) = Cell (toNoun x) (toNoun y)

instance (ToNoun a, ToNoun b, ToNoun c) => ToNoun (a, b, c) where
  toNoun (x, y, z) = Cell (toNoun x)
                   $ Cell (toNoun y) (toNoun z)

instance (ToNoun a, ToNoun b, ToNoun c, ToNoun d) => ToNoun (a, b, c, d) where
  toNoun (x, y, z, a) = Cell (toNoun x)
                      $ Cell (toNoun y)
                      $ Cell (toNoun z) (toNoun a)

instance ToNoun a => ToNoun [a] where
  toNoun xs = fromList (toNoun <$> xs)
