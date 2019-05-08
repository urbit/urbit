-- TODO Handle comments

module Language.Hoon.AST.Parser where

import Language.Hoon.AST.Types
import ClassyPrelude hiding (head, many, some, try)
import Control.Lens
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.State.Lazy
import Data.List.NonEmpty (NonEmpty(..))

import Data.Void        (Void)
import Prelude          (head)
import Text.Format.Para (formatParas)

import qualified Data.MultiMap     as MM
import qualified Data.Text         as T
import qualified Data.Text.Lazy    as LT
import qualified Data.Text.Lazy.IO as LT
import qualified Prelude


-- Parser Monad ------------------------------------------------------------------------------------

data Mode = Wide | Tall
  deriving (Eq, Ord, Show)

type Parser = StateT Mode (Parsec Void Text)

withLocalState :: Monad m => s -> StateT s m a -> StateT s m a
withLocalState val x = do { old <- get; put val; x <* put old }

inWideMode :: Parser a -> Parser a
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

limb ∷ Parser (Either Nat Sym)
limb = (Right <$> sym) <|> (char '+' >> Left <$> nat)

wing ∷ Parser Wing
wing =
    subjt <|> limbs
  where
    subjt ∷ Parser Wing
    subjt = pure [] <* char '.'
    limbs ∷ Parser Wing
    limbs = do s  ← limb
               ss ← many (char '.' >> limb)
               pure (s:ss)

tape ∷ Parser Text
tape = do
  between (char '"') (char '"') $
    pack <$> many (label "tape char" (anySingleBut '"'))

cord ∷ Parser Text
cord = do
  between (char '\'') (char '\'') $
    pack <$> many (label "cord char" (anySingleBut '\''))

literal ∷ Parser Hoon
literal = choice
  [ Atom     <$> atom
  , Wing     <$> wing
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

inc ∷ Parser Hoon -- +(3)
inc = do
  string "+("
  h ← hoon
  char ')'
  pure h

equals ∷ Parser (Hoon, Hoon) -- =(3 4)
equals = do
  string "=("
  x ← hoon
  ace
  y ← hoon
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

irregular ∷ Parser Hoon
irregular =
  inWideMode $
    choice [ Tupl            <$> tuple hoon
           , IncrIrr         <$> inc
           , uncurry IsEqIrr <$> equals
           ]

-- Runes -------------------------------------------------------------------------------------------

cRune ∷ (Map Sym Hoon → a) → Parser a
cRune f = do
    mode ← get
    guard (mode == Tall)
    gap
    f . mapFromList <$> arms -- TODO Complain about duplicated arms
  where
    arms :: Parser [(Sym, Hoon)]
    arms = many arm <* string "--"

    arm :: Parser (Sym, Hoon)
    arm = do
      string "++"
      gap
      s ← sym
      gap
      h ← hoon
      gap
      pure (s, h)

data Skin

rune ∷ Parser Hoon
rune = runeSwitch [ ("|=", rune2 BarTis hoon hoon)
                  , ("|-", rune1 BarHep hoon)
                  , (":-", rune2 ColHep hoon hoon)
                  , (":+", rune3 ColLus hoon hoon hoon)
                  , (":^", rune4 ColKet hoon hoon hoon hoon)
                  , (":*", runeN ColTar hoon)
                  , (":~", runeN ColSig hoon)
                  , ("^-", rune2 KetHep spec hoon)
                  , ("=<", rune2 TisGal hoon hoon)
                  , ("=>", rune2 TisGar hoon hoon)
                  , ("?:", rune3 WutCol hoon hoon hoon)
                  , ("?=", rune2 WutTis spec hoon)
                  , ("?@", rune3 WutPat hoon hoon hoon)
                  , ("?^", rune3 WutKet hoon hoon hoon)
                  , (".+", rune1 Incr hoon)
                  , (".=", rune2 IsEq hoon hoon)
                  , ("^=", rune2 KetTis sym hoon)
                  , ("=.", rune3 TisDot wing hoon hoon)
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

colInfix ∷ Parser Hoon
colInfix = do
  x ← try (hoonNoInfix <* char ':')
  y ← hoon
  pure (ColOp x y)

faceOp ∷ Parser Hoon
faceOp = FaceOp <$> try (sym <* char '=')
                <*> hoon

infixOp ∷ Parser Hoon
infixOp = do
  inWideMode (colInfix <|> faceOp)

-- Hoon Parser -------------------------------------------------------------------------------------

hoonNoInfix ∷ Parser Hoon
hoonNoInfix = irregular <|> rune <|> literal

hoon ∷ Parser Hoon
hoon = infixOp <|> hoonNoInfix

-- Entry Point -------------------------------------------------------------------------------------

hoonFile = do
  option () whitespace
  h ← hoon
  option () whitespace
  eof
  pure h

parse :: Text -> Either Text Hoon
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

specTuple ∷ Parser Spec
specTuple = tuple spec >>= \case
  []   -> mzero
  x:xs -> pure (STuple (x :| xs))

specFace ∷ Parser Spec
specFace = SFaceOp <$> try (sym <* char '=') <*> spec

specIrregular ∷ Parser Spec
specIrregular = inWideMode (specTuple <|> specFace)

spec :: Parser Spec
spec = specIrregular <|> specRune <|> fmap SBase base

specRune ∷ Parser Spec
specRune = choice
  [ string "$:" >> runeNE SBucCol spec
  , string "$-" >> rune2  SBucHep spec spec
  , string "$=" >> rune2  SBucTis sym spec
  , string "$?" >> runeNE SBucWut spec
  , string "$@" >> rune2  SBucPat spec spec
  , string "$^" >> rune2  SBucKet spec spec
  , string "$%" >> runeNE SBucCen spec
  ]
