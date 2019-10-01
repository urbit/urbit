module Untyped.Parser where

import ClassyPrelude hiding (head, many, some, try)
import Control.Lens
import GHC.Natural

import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.State.Lazy

import Data.List.NonEmpty (NonEmpty(..))
import Data.Void          (Void)
import Prelude            (head)
import Text.Format.Para   (formatParas)

import qualified Data.MultiMap     as MM
import qualified Data.Text         as T
import qualified Data.Text.Lazy    as LT
import qualified Data.Text.Lazy.IO as LT
import qualified Prelude


-- Types -----------------------------------------------------------------------

type Nat  = Int
type Sym  = String


-- CST -------------------------------------------------------------------------

data Pat
    = PatTar
    | PatNat Nat
  deriving (Eq, Ord, Show)

data CST
    = WutCol CST CST CST          --  ?:(c t f)
    | WutPat CST CST CST          --  ?@(c t f)
    | WutKet CST CST CST          --  ?^(c t f)
    | WutPam CST CST              --  ?&(c cs ...)
    | WutBar CST CST              --  ?|(c cs ...)
    | WutHep CST [(Pat, CST)]     --  ?-(c p e p e ...)
    | TisFas Sym CST CST          --  =/(x 3 x)
    | ColHep CST CST              --  :-(a b)
    | ColLus CST CST CST          --  :+(a b c)
    | ColKet CST CST CST CST      --  :^(a b c d)
    | ColTar [CST]                --  :*(a as ...)
    | ColSig [CST]                --  :~(a as ...)
    | BarTis Sym CST              --  |=(s h)
    | BarHep Sym Sym CST CST      --  |-(rec var init body)
    | BarCen [(Pat, CST)]         --  |%  %a  3  ==
    | CenHep CST CST              --  %-  f  x
    | CenDot CST CST              --  %.  x  f
    | DotDot Sym CST              --  ..  $  f
    | ZapZap                      --  !!
    | Tupl [CST]                  --  [a b ...]
    | Var  Sym                    --  a
    | Atom Nat                    --  3
    | Tag Text                    --  %asdf
    | Cord Text                   --  'cord'
    | Tape Text                   --  "tape"
    | Incr CST                    --  .+(3)
    | IncrIrr CST                 --  +(3)
    | AppIrr CST CST              --  (x y)
    | IsEq CST CST                --  .=(3 4)
    | IsEqIrr CST CST             --  =(3 4)
    | Pam                         --  &
    | Bar                         --  |
    | Yes                         --  %.y
    | No                          --  %.n
    | Sig                         --  ~
  deriving (Eq, Ord, Show)

-- Parser Monad ----------------------------------------------------------------

data Mode = Wide | Tall
  deriving (Eq, Ord, Show)

type Parser = StateT Mode (Parsec Void Text)

withLocalState :: Monad m => s -> StateT s m a -> StateT s m a
withLocalState val x = do { old <- get; put val; x <* put old }

inWideMode :: Parser a -> Parser a
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
whitespace = ace <|> void gap


-- Literals --------------------------------------------------------------------

alpha ∷ Parser Char
alpha = oneOf (['a'..'z'] ++ ['A'..'Z'])

sym ∷ Parser Sym
sym = bucSym <|> some alpha
  where bucSym = char '$' *> pure ""

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

literal ∷ Parser CST
literal = choice
  [ Var      <$> sym
  , Atom     <$> atom
  , pure Yes <*  string "%.y"
  , pure No  <*  string "%.n"
  , pure Pam <*  char '&'
  , pure Bar <*  char '|'
  , pure Sig <*  char '~'
  , Cord     <$> cord
  , Tape     <$> tape
  ]


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
                       term    = string "==" *> pure []
        wide = pal *> option [] elems <* par
                 where elems = (:) <$> elem <*> many (ace >> elem)

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

tuple ∷ ∀a. Parser a → Parser [a]
tuple p = char '[' >> elems
  where
    xs ∷ Parser [a]
    xs = do { x ← p; (x:) <$> tail }

    tail ∷ Parser [a]
    tail = (pure [] <* char ']')
       <|> (ace >> elems)

    elems ∷ Parser [a]
    elems = (pure [] <* char ']') <|> xs

appIrr :: Parser CST
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

cRune ∷ (Map Sym CST → a) → Parser a
cRune f = do
    mode ← get
    guard (mode == Tall)
    gap
    f . mapFromList <$> arms -- TODO Complain about duplicated arms
  where
    arms :: Parser [(Sym, CST)]
    arms = many arm <* string "--"

    arm :: Parser (Sym, CST)
    arm = do
      string "++"
      gap
      s ← sym
      gap
      h ← cst
      gap
      pure (s, h)

rune ∷ Parser CST
rune = runeSwitch [ ("|=", rune2 BarTis sym cst)
                  , ("|-", rune4 BarHep sym sym cst cst)
                  , (":-", rune2 ColHep cst cst)
                  , (":+", rune3 ColLus cst cst cst)
                  , (":^", rune4 ColKet cst cst cst cst)
                  , (":*", runeN ColTar cst)
                  , (":~", runeN ColSig cst)
                  , ("%-", rune2 CenHep cst cst)
                  , ("%.", rune2 CenDot cst cst)
                  , ("?:", rune3 WutCol cst cst cst)
                  , ("?@", rune3 WutPat cst cst cst)
                  , ("?^", rune3 WutKet cst cst cst)
                  , (".+", rune1 Incr cst)
                  , (".=", rune2 IsEq cst cst)
--                , ("|%", cRune BarCen)
                  ]

runeSwitch ∷ [(Text, Parser a)] → Parser a
runeSwitch = choice . fmap (\(s, p) → string s *> p)


-- CST Parser ------------------------------------------------------------------

cst ∷ Parser CST
cst = irregular <|> rune <|> literal


-- Entry Point -----------------------------------------------------------------

hoonFile = do
  option () whitespace
  h ← cst
  option () whitespace
  eof
  pure h

parse :: Text -> Either Text CST
parse txt =
  runParser (evalStateT hoonFile Tall) "stdin" txt & \case
    Left  e -> Left (pack $ errorBundlePretty e)
    Right x -> pure x

parseHoonTest ∷ Text → IO ()
parseHoonTest = parseTest (evalStateT hoonFile Tall)

main ∷ IO ()
main = (head <$> getArgs) >>= parseHoonTest
