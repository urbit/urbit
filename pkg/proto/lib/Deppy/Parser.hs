module Deppy.Parser where

import ClassyPrelude hiding (head, many, some, try, init, last)
import Control.Arrow ((>>>))
import Control.Lens
import Numeric.Natural

import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.State.Lazy

import Data.Void          (Void)
import Prelude            (head, init, last)

import qualified Prelude

import Deppy.CST
import qualified Deppy.Hoon as Hoon
import qualified Deppy.Core as Core
import Deppy.Showings ()
import SimpleNoun (textToAtom)

-- Types -----------------------------------------------------------------------

type Nat  = Natural
type Sym  = Text

-- Marvelous convenience -------------------------------------------------------

instance IsString CST where
  fromString = pack >>> parseCst >>> \case
    Right c -> c
    Left er -> error $ unpack er

instance IsString (Hoon.Hoon Text) where
  fromString = abstractify . fromString

instance IsString (Core.Exp Text) where
  fromString = Hoon.desugar . fromString

-- Parser Monad ----------------------------------------------------------------

data Mode = Wide | Tall
  deriving (Eq, Ord, Show)

type Parser = StateT Mode (Parsec Void Text)

withLocalState ∷ Monad m => s → StateT s m a → StateT s m a
withLocalState val x = do { old <- get; put val; x <* put old }

inWideMode ∷ Parser a → Parser a
inWideMode = withLocalState Wide

-- Simple Lexers ---------------------------------------------------------------

ace, pal, par ∷ Parser ()
ace = void (char ' ')
pal = void (char '(')
par = void (char ')')

gap ∷ Parser ()
gap = choice [ char ' ' >> void (some spaceChar)
             , newline  >> void (many spaceChar)
             ]

whitespace ∷ Parser ()
whitespace = ace <|> gap


-- Literals --------------------------------------------------------------------

alpha ∷ Parser Char
alpha = oneOf (['a'..'z'] ++ ['A'..'Z'])

sym ∷ Parser Sym
sym = pack <$> some alpha

atom ∷ Parser Nat
atom = do
  init ← some digitChar
  rest ← many (char '.' *> some digitChar)
  guard True -- TODO Validate '.'s
  pure (Prelude.read $ concat $ init:rest)

nat ∷ Parser Nat
nat = Prelude.read <$> some digitChar

tape ∷ Parser Text
tape = do
  between (char '"') (char '"') $
    pack <$> many (label "tape char" (anySingleBut '"'))

cord ∷ Parser Text
cord = do
  between (char '\'') (char '\'') $
    pack <$> many (label "cord char" (anySingleBut '\''))

tag ∷ Parser Text
tag = try (char '%' >> sym)

-- Grammar ---------------------------------------------------------------------

cst :: Parser CST
cst = irregular <|> rune

irregular :: Parser CST
irregular =
  inWideMode $ choice
    [ Col <$> try (textToAtom <$> sym <* char ':') <*> cst
    , notAllow Fun <$> grouped "<|" " " "|>" binder
    , notAllow Cel <$> grouped "[|" " " "|]" binder
    , Wut . singleton <$> try tagTy
    , Wut . setFromList <$> grouped "?(" " " ")" (atom <|> textToAtom <$> sym)
    , notAllow Lam <$> grouped "<" " " ">" binder
    , Cns <$> grouped "[" " " "]" cst
    , Tag . textToAtom <$> tag
    , Tag <$> atom
    , App <$> grouped "(" " " ")" cst
    , Hed <$> (string "-." *> cst)
    , Tal <$> (string "+." *> cst)
    , The <$> (char '`' *> cst <* char '`') <*> cst
    -- , Fas TODO
    , Cls . mapFromList <$> grouped "{|" ", " "|}" entry
    , Obj . mapFromList <$> grouped "{" ", " "}" entry
    , Hax <$ char '#'
    , Var <$> sym
    , Tag 0 <$ char '~'
    ]
  where
    tagTy = char '$' *> (atom <|> textToAtom <$> sym)
    entry = do
      tag <- atom <|> textToAtom <$> sym
      char ' '
      c <- cst
      pure (tag, c)

rune ∷ Parser CST
rune = runeSwitch
  [ ("$%", runeJogging (HaxBuc . mapFromList) (textToAtom <$> sym) cst)
  , ("$=", runeJogging (HaxCen . mapFromList) (textToAtom <$> sym) cst)
  , ("$:", runeN (notAllow HaxCol) binder)
  , ("$-", runeN (notAllow HaxHep) binder)
  , ("|%", barCen)
  , ("|=", runeN (notAllow BarTis) binder)
  , ("%-", rune2 CenHep cst cst)
  , ("%.", rune2 CenDot cst cst)
  , (":-", rune2 ColHep cst cst)
  , (":*", runeN ColTar cst)
  , ("=/", rune3 TisFas sym cst cst)
  , ("..", rune2 DotDot binder cst)
  , (".<", rune1 DotGal cst)
  , (".>", rune1 DotGar cst)
  , ("^/", rune2 KetFas cst cst)
  , ("^-", rune2 KetHep cst cst)
  , ("?%", runeJogging1 wutCen cst (textToAtom <$> tag <|> atom) cst)
  ]
  where
    wutCen c cs = WutCen c (mapFromList cs)

runeSwitch ∷ [(Text, Parser a)] → Parser a
runeSwitch = choice . fmap (\(s, p) → string s *> p)

-- Custom runes ----------------------------------------------------------------

barCen :: Parser CST
barCen = do
  mode <- get
  guard (mode == Tall)
  gap
  arms <- many do
    string "++"
    gap
    name <- sym
    gap
    body <- cst
    gap
    pure (textToAtom name, body)
  string "--"
  pure $ BarCen $ mapFromList arms

-- Groups and binders ----------------------------------------------------------

grouped :: Text -> Text -> Text -> Parser a -> Parser [a]
grouped open delim close elem = string open >> body
  where
    body = shut <|> (:) <$> elem <*> rest
    rest = many (string delim *> elem) <* shut
    shut = string close $> []

-- | A binder is a cst prefixed by an optional sym-and-'/'
binder :: Parser Binder
binder = (,) <$> parseMay tag <*> inWideMode cst
  where
    tag = try $ sym <* char '/'

parseMay :: Parser a -> Parser (Maybe a)
parseMay p = option Nothing (Just <$> p)

-- | Binding forms consist of a sequence of binders followed by a cst
-- This is implemented by parsing a sequence of binders, and disallowing
-- the binding prefix on the last element.
notAllow con bs = con (init bs) (go $ last bs)
  where
    go (Nothing, c) = c
    go (Just _, _) = error "syntax: a binder is not allowed in final position"

-- Rune Helpers ----------------------------------------------------------------

{-
    - If the parser is in `Wide` mode, only accept the `wide` form.
    - If the parser is in `Tall` mode, either
      - accept the `tall` form or:
      - swich to `Wide` mode and then accept the wide form.
-}
parseRune ∷ Parser a → Parser a → Parser a
parseRune tall wide = get >>= \case
  Wide → wide
  Tall → tall <|> inWideMode wide

rune0 ∷ a → Parser a
rune0 = pure

rune1 ∷ (a→b) → Parser a → Parser b
rune1 node x = parseRune tall wide
  where tall = do gap; p←x;      pure (node p)
        wide = do pal; p←x; par; pure (node p)

rune2 ∷ (a→b→c) → Parser a → Parser b → Parser c
rune2 node x y = parseRune tall wide
  where tall = do gap; p←x; gap; q←y;      pure (node p q)
        wide = do pal; p←x; ace; q←y; par; pure (node p q)

rune3 ∷ (a→b→c→d) → Parser a → Parser b → Parser c → Parser d
rune3 node x y z = parseRune tall wide
  where tall = do gap; p←x; gap; q←y; gap; r←z;      pure (node p q r)
        wide = do pal; p←x; ace; q←y; ace; r←z; par; pure (node p q r)

rune4 ∷ (a→b→c→d→e) → Parser a → Parser b → Parser c → Parser d → Parser e
rune4 node x y z g = parseRune tall wide
  where tall = do gap; p←x; gap; q←y; gap; r←z; gap; s←g; pure (node p q r s)
        wide = do pal; p←x; ace; q←y; ace; r←z; ace; s←g; pure (node p q r s)

runeN ∷ ([a]→b) → Parser a → Parser b
runeN node elem = node <$> parseRune tall wide
  where tall = gap >> elems
                 where elems   = term <|> elemAnd
                       elemAnd = do x ← elem; gap; xs ← elems; pure (x:xs)
                       term    = string "==" $> []
        wide = pal *> option [] elems <* par
                 where elems = (:) <$> elem <*> many (ace >> elem)

runeJogging :: ([(a, b)] -> r) -> Parser a -> Parser b -> Parser r
runeJogging node a b = node <$> parseRune tall wide
  where
    tall = gap *> many ((,) <$> (a <* gap) <*> (b <* gap)) <* string "=="
    wide = grouped "(" ", " ")" ((,) <$> (a <* ace) <*> b)

runeJogging1 :: (a -> [(b, c)] -> r)
             -> Parser a -> Parser b -> Parser c -> Parser r
runeJogging1 node a b c = parseRune tall wide
  where
    tall = do
      gap
      x <- a
      gap
      elems <- many do
        y <- b
        gap
        z <- c
        gap
        pure (y, z)
      string "=="
      pure $ node x elems
    wide = do
        string "("
        x <- a
        elems <- wideBody <|> (char ')' $> [])
        pure $ node x elems
      where
        wideBody = do
          hack <- grouped "; " ", " ")" do
            y <- b
            ace
            z <- c
            pure (y, z)
          case hack of
            [] -> fail "leave off '; ' for empty jogging segment"
            _  -> pure hack

-- Entry Point -----------------------------------------------------------------

hoonFile = do
  option () whitespace
  h ← cst
  option () whitespace
  eof
  pure h
parseCst ∷ Text → Either Text CST
parseCst txt =
  runParser (evalStateT hoonFile Tall) "stdin" txt & \case
    Left  e → Left (pack $ errorBundlePretty e)
    Right x → pure x

parseHoonTest ∷ Text → IO ()
parseHoonTest = parseTest (evalStateT hoonFile Tall)

main ∷ IO ()
main = (head <$> getArgs) >>= parseHoonTest
