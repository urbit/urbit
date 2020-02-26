module Moon.Parser where

import ClassyPrelude hiding (many, last, init, try, exp, some)
import Bound
import Control.Lens
import Control.Monad.State.Lazy
import Moon.AST
import Numeric.Natural
import Text.Megaparsec
import Text.Megaparsec.Char

import Control.Arrow ((>>>))
import Data.Void     (Void)
import Prelude       (head, init, last)
import Urbit.Atom    (utf8Atom)

import qualified Prelude


-- Types -----------------------------------------------------------------------

type Sym  = Text


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

-- TODO - only in middle, support prime
alphaChar :: [Char]
alphaChar = ['a'..'z'] <> ['A'..'Z']

alpha ∷ Parser Char
alpha = oneOf alphaChar

symChar = oneOf (['-'] ++ alphaChar ++ ['0'..'9'])

sym ∷ Parser Sym
sym = (pack <$> ((:) <$> alpha <*> many symChar))

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

exp :: Parser AST
exp = try rune <|> irregular

cns ∷ [AST] → AST
cns []     = ASig
cns [x]    = x
cns (x:xs) = ACon x (cns xs)

appN ∷ [AST] → AST
appN []     = ASig
appN [x]    = x
appN (x:xs) = go x xs
 where
  go acc []     = acc
  go acc (x:xs) = go (AApp acc x) xs

lam ∷ Parser AST
lam = do
  char '<'
  binds ← some (try (sym <* char ' '))
  body  ← exp
  char '>'
  pure (lambdify binds body)

lambdify ∷ [Text] → AST → AST
lambdify binds body = go body binds
 where
  go acc []     = acc
  go acc (b:bs) = ALam b (go acc bs)

irregular :: Parser AST
irregular =
  inWideMode $ choice
    [ lam
    , cns <$> grouped "[" " " "]" exp
    , ALit <$> atom
    , AStr <$> cord
    , appN <$> grouped "(" " " ")" exp
    , AVar <$> sym
    , ASig <$ char '~'
    ]
  where
    tagTy = char '$' *> (atom <|> utf8Atom <$> sym)
    entry = do
      tag <- atom <|> utf8Atom <$> sym
      char ' '
      c <- exp
      pure (tag, c)

app3 x y z = appN [x, y, z]
app4 x y z p = appN [x, y, z, p]

rune :: Parser AST
rune = runeSwitch
  [ ("~/", rune3 AJet nat sym exp)
  , ("|=", rune2 lambdify lamArgs exp)
  , ("=/", rune3 ALet sym exp exp)
  , ("..", rune2 AFix sym exp)
  , (":-", rune2 ACon exp exp)
  , (":*", runeN cns exp)
  , ("%-", rune2 AApp exp exp)
  , ("%+", rune3 app3 exp exp exp)
  , ("%^", rune4 app4 exp exp exp exp)
  , ("%*", runeN appN exp)
  , ("%.", rune2 (flip AApp) exp exp)
  , ("?:", rune3 AIff exp exp exp)
  ]
  where lamArgs = grouped "(" " " ")" sym <|> (: []) <$> sym

{-
  [ ("$:", runeN (notAllow HaxCol) binder)
  , (".+", rune1 DotLus exp)
  , (".=", rune2 DotTis exp exp)
--, ("?-", runeJogging1 wutHep exp tagPat exp)
  ]
    tagPat = Atom.utf8Atom <$> tag <|> atom
--    wutHep c cs = Cas c (mapFromList cs)
    celPat = char '[' *> ((,) <$> tagPat <*> (ace *> sym)) <* char ']'
    wutHax c stuff =
      WutHax c (mapFromList $ map (\((a, v), d) -> (a, (v, d))) stuff)
-}

runeSwitch ∷ [(Text, Parser a)] → Parser a
runeSwitch = choice . fmap (\(s, p) → string s *> p)

-- Groups and binders ----------------------------------------------------------

grouped :: Text -> Text -> Text -> Parser a -> Parser [a]
grouped open delim close elem = string open >> body
  where
    body = shut <|> (:) <$> elem <*> rest
    rest = many (string delim *> elem) <* shut
    shut = string close $> []

binder :: Parser a
binder = error "binder"
{-
-- | A binder is a exp prefixed by an optional sym-and-'/'
binder = (,) <$> parseMay tag <*> {-inWideMode-} exp
  where
    tag = try $ sym <* char '/'
-}

parseMay :: Parser a -> Parser (Maybe a)
parseMay p = option Nothing (Just <$> p)

-- | Binding forms consist of a sequence of binders followed by a exp
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
  h ← exp
  option () whitespace
  eof
  pure h

parseAST ∷ Text → Either Text AST
parseAST txt =
  runParser (evalStateT hoonFile Tall) "stdin" txt & \case
    Left  e → Left (pack $ errorBundlePretty e)
    Right x → pure x

parseHoonTest ∷ Text → IO ()
parseHoonTest = parseTest (evalStateT hoonFile Tall)

main ∷ IO ()
main = (Prelude.head <$> getArgs) >>= parseHoonTest
