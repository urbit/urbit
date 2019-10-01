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
type Wing = [Either Nat Sym]


-- AST -------------------------------------------------------------------------

data Base = BVoid | BNull | BFlag | BNoun | BCell | BAtom
  deriving (Eq, Ord, Show)

data AST
    = WutCol AST AST AST          --  ?:(c t f)
    | WutPat AST AST AST          --  ?@(c t f)
    | WutKet AST AST AST          --  ?^(c t f)
    | KetTis Sym AST              --  ^=(x 3)
    | ColHep AST AST              --  :-(a b)
    | ColLus AST AST AST          --  :+(a b c)
    | ColKet AST AST AST AST      --  :^(a b c d)
    | ColTar [AST]                --  :*(a as ...)
    | ColSig [AST]                --  :~(a as ...)
    | TisGal AST AST              --  =<(a b)
    | TisGar AST AST              --  =>(a b)
    | BarTis AST AST              --  |=(s h)
    | BarHep AST                  --  |-(a)
    | TisDot Sym AST AST          --  =.(a 3 a)
    | BarCen (Map Sym AST)        --  |%  ++  a  3  --
    | ColOp AST AST               --  [+ -]:[3 4]
    | Tupl [AST]                  --  [a b]
    | FaceOp Sym AST              --  x=y
    | Wing Wing                   --  ., a, a.b
    | Atom Nat                    --  3
    | Cord Text                   --  'cord'
    | Tape Text                   --  "tape"
    | Incr AST                    --  .+(3)
    | IncrIrr AST                 --  +(3)
    | IsEq AST AST                --  .=(3 4)
    | IsEqIrr AST AST             --  =(3 4)
    | Lus                         --  +
    | Hep                         --  -
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

literal ∷ Parser AST
literal = choice
  [ Atom     <$> atom
  , pure Yes <*  string "%.y"
  , pure No  <*  string "%.n"
  , pure Pam <*  char '&'
  , pure Bar <*  char '|'
  , pure Sig <*  char '~'
  , pure Lus <*  char '+'
  , pure Hep <*  char '-'
  , Cord     <$> cord
  , Tape     <$> tape
  ]


-- Rune Helpers ------------------------------------------------------------------------------------

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

-- Irregular Syntax --------------------------------------------------------------------------------

inc ∷ Parser AST -- +(3)
inc = do
  string "+("
  h ← ast
  char ')'
  pure h

equals ∷ Parser (AST, AST) -- =(3 4)
equals = do
  string "=("
  x ← ast
  ace
  y ← ast
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

irregular ∷ Parser AST
irregular =
  inWideMode $
    choice [ Tupl            <$> tuple ast
           , IncrIrr         <$> inc
           , uncurry IsEqIrr <$> equals
           ]

-- Runes -----------------------------------------------------------------------

cRune ∷ (Map Sym AST → a) → Parser a
cRune f = do
    mode ← get
    guard (mode == Tall)
    gap
    f . mapFromList <$> arms -- TODO Complain about duplicated arms
  where
    arms :: Parser [(Sym, AST)]
    arms = many arm <* string "--"

    arm :: Parser (Sym, AST)
    arm = do
      string "++"
      gap
      s ← sym
      gap
      h ← ast
      gap
      pure (s, h)

data Skin

rune ∷ Parser AST
rune = runeSwitch [ ("|=", rune2 BarTis ast ast)
                  , ("|-", rune1 BarHep ast)
                  , (":-", rune2 ColHep ast ast)
                  , (":+", rune3 ColLus ast ast ast)
                  , (":^", rune4 ColKet ast ast ast ast)
                  , (":*", runeN ColTar ast)
                  , (":~", runeN ColSig ast)
                  , ("=<", rune2 TisGal ast ast)
                  , ("=>", rune2 TisGar ast ast)
                  , ("?:", rune3 WutCol ast ast ast)
                  , ("?@", rune3 WutPat ast ast ast)
                  , ("?^", rune3 WutKet ast ast ast)
                  , (".+", rune1 Incr ast)
                  , (".=", rune2 IsEq ast ast)
                  , ("^=", rune2 KetTis sym ast)
                  , ("=.", rune3 TisDot sym ast ast)
                  , ("|%", cRune BarCen)
                  ]

runeSwitch ∷ [(Text, Parser a)] → Parser a
runeSwitch = choice . fmap (\(s, p) → string s *> p)

-- runeSwitch ∷ [(String, Parser a)] → Parser a
-- runeSwitch = parseBasedOnRune
--            . fmap (\([x,y], p) → (x, (y,p)))
--   where
--     parseBasedOnRune ∷ [(Char, (Char, Parser a))] → Parser a
--     parseBasedOnRune = combine . restructure
--       where combine     = lexThen . overSnd lexThen
--             overSnd f   = fmap (\(x,y) → (x,f y))
--             lexThen     = choice . fmap (\(x,y) → char x *> y)
--             restructure = MM.assocs
--                         . MM.fromList

-- Infix Syntax ------------------------------------------------------------------------------------

colInfix ∷ Parser AST
colInfix = do
  x ← try (astNoInfix <* char ':')
  y ← ast
  pure (ColOp x y)

faceOp ∷ Parser AST
faceOp = FaceOp <$> try (sym <* char '=')
                <*> ast

infixOp ∷ Parser AST
infixOp = do
  inWideMode (colInfix <|> faceOp)

-- AST Parser -------------------------------------------------------------------------------------

astNoInfix ∷ Parser AST
astNoInfix = irregular <|> rune <|> literal

ast ∷ Parser AST
ast = infixOp <|> astNoInfix

-- Entry Point -------------------------------------------------------------------------------------

hoonFile = do
  option () whitespace
  h ← ast
  option () whitespace
  eof
  pure h

parse :: Text -> Either Text AST
parse txt =
  runParser (evalStateT hoonFile Tall) "stdin" txt & \case
    Left  e -> Left (pack $ errorBundlePretty e)
    Right x -> pure x

parseHoonTest ∷ Text → IO ()
parseHoonTest = parseTest (evalStateT hoonFile Tall)

main ∷ IO ()
main = (head <$> getArgs) >>= parseHoonTest


-- Parse Spec ------------------------------------------------------------------

base :: Parser Base
base = choice [ BVoid <$ char '!'
              , BNull <$ char '~'
              , BFlag <$ char '?'
              , BNoun <$ char '*'
              , BCell <$ char '^'
              , BAtom <$ char '@'
              ]
