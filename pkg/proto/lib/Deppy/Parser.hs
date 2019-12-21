module Deppy.Parser where

import ClassyPrelude hiding (head, many, some, try, init, last)
import Control.Lens
import Numeric.Natural

import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.State.Lazy

import Data.List.NonEmpty (NonEmpty(..))
import Data.Void          (Void)
import Prelude            (init, last)

import qualified Prelude

import Deppy.CST
import SimpleNoun (textToAtom)
import qualified Noun as N


-- Types -----------------------------------------------------------------------

type Nat  = Natural
type Sym  = Text

-- CST -------------------------------------------------------------------------

instance Show CST where
  show = \case
      Var t -> unpack t
      Hax -> "$"
      Fun bs x -> "$<" <> showBound bs x <> ">"
      Cel bs x -> "$[" <> showBound bs x <> "]"
      Wut (setToList -> [x]) -> showTag "$" "$" x
      Wut (setToList -> xs) -> "?(" <> intercalate " " (showTag "$" "$" <$> xs) <> ")"
      Lam bs x -> "<" <> showBound bs x <> ">"
      Cns xs -> "[" <> showGroup xs <> "]"
      Tag a -> showTag "%" "" a
      App xs -> "(" <> showGroup xs <> ")"
      Hed x -> "-." <> show x
      Tal x -> "+." <> show x
      The x y -> "`" <> show x <> "`" <> show y
      Fas x y -> show x <> "/" <> show y
      Obj (mapToList -> cs) -> "{" <> showEnts cs <> "}"
      Cls (mapToList -> tcs) -> "${" <> showEnts tcs <> "}"
      Col a x -> showTag "" "" a <> ":" <> show x
      HaxBuc (mapToList -> cs) -> "$%(" <> showEnts cs <> ")"
      HaxCen (mapToList -> cs) -> "$`(" <> showEnts cs <> ")"
      HaxCol bs x -> "$:(" <> showBound bs x <> ")"
      HaxHep bs x -> "$-(" <> showBound bs x <> ")"
      BarCen (mapToList -> cs) -> "|%(" <> showEnts cs <> ")"
      BarTis bs x -> "|=(" <> showBound bs x <> ")"
      CenDot x y -> "%.(" <> show x <> " " <> show y <> ")"
      CenHep x y -> "%-(" <> show x <> " " <> show y <> ")"
      ColHep x y -> ":-(" <> show x <> " " <> show y <> ")"
      ColTar xs -> ":*(" <> showGroup xs <> ")"
      TisFas t x y -> "=/(" <> unpack t <> show x <> " " <> show y <> ")"
      DotDot x y -> "..(" <> showBinder x <> " " <> show y <> ")"
      KetFas x y -> "^/(" <> show x <> " " <> show y <> ")"
      KetHep x y -> "^-(" <> show x <> " " <> show y <> ")"
      WutCen x (mapToList -> cs) -> "?%(" <> show x <> "; " <> showEnts' cs <> ")"
    where
      showEnts  xs = intercalate ", " (showEnt "" <$> xs)
      showEnts' xs = intercalate ", " (showEnt "%" <$> xs)
      showEnt s (x, y) = showTag s "" x <> " " <> show y
      showGroup xs = intercalate " " (show <$> xs)
      showTag p1 p2 x = N.fromNoun (N.A x) &
        \case
          Just (N.Cord x) | okay x -> p1 <> unpack x
          _ -> p2 <> show x
      okay = all (flip elem ['a'..'z'])
      showBound bs x =  showBinders bs <> " " <> show x
      showBinders bs = intercalate " " (showBinder <$> bs)
      showBinder (Nothing, x) = show x
      showBinder (Just t, x) = unpack t <> "/" <> show x
    

-- Parser Monad ----------------------------------------------------------------

data Mode = Wide | Tall
  deriving (Eq, Ord, Show)

type Parser = StateT Mode (Parsec Void Text)

withLocalState ∷ Monad m => s → StateT s m a → StateT s m a
withLocalState val x = do { old <- get; put val; x <* put old }

inWideMode ∷ Parser a → Parser a
inWideMode = withLocalState Wide

ace, pal, par ∷ Parser ()
ace = void (char ' ')
pal = void (char '(')
par = void (char ')')

-- Simple Lexers ---------------------------------------------------------------

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

{-
literal ∷ Parser CST
literal = choice
  [ Yes  <$  string "%.y"
  , No   <$  string "%.n"
  , Var  <$> sym
  , Atom <$> atom
  , Pam  <$  char '&'
  , Bar  <$  char '|'
  , Sig  <$  char '~'
  , Tag  <$> tag
  , Cord <$> cord
  , Tape <$> tape
  ]
-}

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

runeJogging :: Parser a -> Parser b -> Parser [(a, b)]
runeJogging a b = parseRune tall wide
  where
    tall = gap *> many ((,) <$> (a <* gap) <*> (b <* gap)) <* string "=="
    wide = grouped "(" ", " ")" ((,) <$> (a <* ace) <*> b)

runeJogging1 :: Parser a -> Parser b -> Parser c -> Parser (a, [(b, c)])
runeJogging1 a b c = parseRune tall wide
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
      pure (x, elems)
    wide = do
      string "("
      x <- a
      elems <- grouped "; " ", " ")" do
        y <- b
        ace
        z <- c
        pure (y, z)
      pure (x, elems)

runeNE ∷ (NonEmpty a → b) → Parser a → Parser b
runeNE node elem = node <$> parseRune tall wide
  where tall = do
          let elems   = term <|> elemAnd
              elemAnd = do x ← elem; gap; xs ← elems; pure (x:xs)
              term    = string "==" *> pure []
          fst <- gap *> elem
          rst <- gap *> elems
          pure (fst :| rst)
        wide = mzero -- No wide form for cores

-- Irregular Syntax ------------------------------------------------------------

{-
inc ∷ Parser CST -- +(3)
inc = do
  string "+("
  h ← cst
  char ')'
  pure h

equals ∷ Parser (CST, CST) -- =(3 4)
equals = do
  string "=("
  x ← cst
  ace
  y ← cst
  char ')'
  pure (x, y)
-}

cst :: Parser CST
cst = irregular <|> rune

grouped :: Text -> Text -> Text -> Parser a -> Parser [a]
grouped open delim close elem = string open >> body
  where
    body = shut <|> (:) <$> elem <*> rest
    rest = many (string delim *> elem) <* shut
    shut = string close $> []

binder :: Parser Binder
binder = (,) <$> parseMay tag <*> inWideMode cst
  where
    tag = try $ sym <* char '/'

parseMay :: Parser a -> Parser (Maybe a)
parseMay p = option Nothing (Just <$> p)

irregular :: Parser CST
irregular =
  inWideMode $ choice
    [ Col <$> try (textToAtom <$> sym <* char ':') <*> cst
    , notAllow Fun <$> grouped "$<" " " ">" binder
    , notAllow Cel <$> grouped "$[" " " "]" binder
    , Wut . singleton . textToAtom <$> try (char '$' *> sym)
    , Wut . setFromList . fmap textToAtom <$> grouped "?(" " " ")" (char '$' *> sym)
    , Hax <$ char '$'
    , notAllow Lam <$> grouped "<" " " ">" binder
    , Cns <$> grouped "[" " " "]" cst
    , Tag . textToAtom <$> tag
    , Tag <$> atom
    , App <$> grouped "(" " " ")" cst
    , Hed <$> (string "-." *> cst)
    , Tal <$> (string "+." *> cst)
    , The <$> (char '`' *> cst <* char '`') <*> cst
    -- , Fas TODO
    , Obj . mapFromList <$> grouped "{" ", " "}" entry
    , Cls . mapFromList <$> grouped "${" ", " "}" entry
    , Var <$> sym
    ]
  where
    entry = do
      tag <- sym
      char ' '
      c <- cst
      pure (textToAtom tag, c)

notAllow con bs = con (init bs) (go $ last bs)
  where
    go (Nothing, c) = c
    go (Just _, _) = error "syntax: a binder is not allowed in final position"

rune ∷ Parser CST
rune = runeSwitch [ ("$%", undefined)
                  , ("|=", runeN (notAllow BarTis) binder)
                  --, ("|-", rune4 BarHep sym sym cst cst)
                  , (":-", rune2 ColHep cst cst)
                  --, (":+", rune3 ColLus cst cst cst)
                  --, (":^", rune4 ColKet cst cst cst cst)
                  , (":*", runeN ColTar cst)
                  --, (":~", runeN ColSig cst)
                  , ("%-", rune2 CenHep cst cst)
                  , ("%.", rune2 CenDot cst cst)
                  , ("..", rune2 DotDot binder cst)
                  --, ("!!", rune0 ZapZap)
                  --, ("?:", rune3 WutCol cst cst cst)
                  --, ("?@", rune3 WutPat cst cst cst)
                  --, ("?&", runeN WutPam cst)
                  --, ("?|", runeN WutBar cst)
                  --, ("?^", rune3 WutKet cst cst cst)
                  , ("=/", rune3 TisFas sym cst cst)
                  --, (".+", rune1 Incr cst)
                  --, (".=", rune2 IsEq cst cst)
                  --, ("?-", wutHep)
                  --, ("|%", barCen)
                  --, ("~/", rune2 SigFas cst cst)
                  ]

runeSwitch ∷ [(Text, Parser a)] → Parser a
runeSwitch = choice . fmap (\(s, p) → string s *> p)

{-
appIrr ∷ Parser CST
appIrr = do
  char '('
  x <- cst
  char ' '
  y <- cst
  char ')'
  pure (AppIrr x y)

irregular ∷ Parser CST
irregular =
  inWideMode $
    choice [ Tupl            <$> tuple cst
           , IncrIrr         <$> inc
           , uncurry IsEqIrr <$> equals
           , appIrr
           ]

-- Runes -----------------------------------------------------------------------

pat ∷ Parser Pat
pat = choice [ PatTag   <$> tag
             , char '*'  $> PatTar
             ]

cases ∷ Parser [(Pat, CST)]
cases = do
    mode ← get
    guard (mode == Tall)
    end <|> lop
  where
    goo = lop <|> end
    end = string "==" $> []
    lop = do { p <- pat; gap; b <- cst; gap; ((p,b):) <$> goo }

wutHep ∷ Parser CST
wutHep = do
    mode ← get
    guard (mode == Tall)
    gap
    ex <- cst
    gap
    cs <- cases
    pure (WutHep ex cs)

barCen ∷ Parser CST
barCen = do
    mode ← get
    guard (mode == Tall)
    gap
    cs <- cases
    pure (BarCen cs)



-- CST Parser ------------------------------------------------------------------

cst ∷ Parser CST
cst = irregular <|> rune <|> literal


-- Entry Point -----------------------------------------------------------------
-}
hoonFile = do
  option () whitespace
  h ← cst
  option () whitespace
  eof
  pure h
parse ∷ Text → Either Text CST
parse txt =
  runParser (evalStateT hoonFile Tall) "stdin" txt & \case
    Left  e → Left (pack $ errorBundlePretty e)
    Right x → pure x

parseHoonTest ∷ Text → IO ()
parseHoonTest = parseTest (evalStateT hoonFile Tall)
{-

main ∷ IO ()
main = (head <$> getArgs) >>= parseHoonTest
-}
