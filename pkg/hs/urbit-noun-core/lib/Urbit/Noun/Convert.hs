{-|
    Framework for writing conversions between types and nouns.
-}
module Urbit.Noun.Convert
    ( ToNoun(toNoun)
    , FromNoun(parseNoun), fromNoun, fromNounErr, fromNounExn
    , Parser(..)
    , ParseStack
    , parseNounUtf8Atom
    , named
    ) where

import ClassyPrelude hiding (hash)

import Control.Monad.Fail (MonadFail (fail))
import Urbit.Noun.Core


-- Types -----------------------------------------------------------------------

type ParseStack = [Text]


-- "Parser" --------------------------------------------------------------------

type Failure a   = ParseStack -> String -> a
type Success a b = a -> b

newtype Parser a = Parser {
  runParser :: forall r. ParseStack -> Failure r -> Success a r -> r
}

named :: Text -> Parser a -> Parser a
named nm (Parser cb) =
    Parser $ \path kf ks -> cb (nm:path) kf ks

instance Monad Parser where
    m >>= g = Parser $ \path kf ks -> let ks' a = runParser (g a) path kf ks
                                       in runParser m path kf ks'
    return = pure

instance MonadFail Parser where
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

--------------------------------------------------------------------------------

fromNoun :: FromNoun a => Noun -> Maybe a
fromNoun n = runParser (parseNoun n) [] onFail onSuccess
  where
    onFail p m  = Nothing
    onSuccess !x = Just x

fromNounErr :: FromNoun a => Noun -> Either ([Text], Text) a
fromNounErr n = runParser (parseNoun n) [] onFail onSuccess
  where
    onFail p m  = Left (p, pack m)
    onSuccess !x = Right x

data BadNoun = BadNoun [Text] String
  deriving (Eq, Ord)

instance Show BadNoun where
  show (BadNoun pax msg) =
    mconcat [ "(BadNoun "
            , show (intercalate "." pax)
            , " "
            , show msg
            , ")"
            ]

instance Exception BadNoun where

fromNounExn :: MonadIO m => FromNoun a => Noun -> m a
fromNounExn n = runParser (parseNoun n) [] onFail onSuccess
  where
    onFail p m  = throwIO (BadNoun p m)
    onSuccess !x = pure x


-- Cord Conversions ------------------------------------------------------------

{-# NOINLINE parseNounUtf8Atom #-}
parseNounUtf8Atom :: Noun -> Parser Text
parseNounUtf8Atom n =
    named "utf8-atom" $ do
      case utf8AtomToText n of
        Left err -> fail (unpack err)
        Right tx -> pure tx
