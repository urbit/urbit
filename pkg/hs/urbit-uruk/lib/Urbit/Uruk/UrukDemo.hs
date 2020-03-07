{-# OPTIONS_GHC -Wall -Werror #-}
{-# OPTIONS_GHC -Werror #-}

{-
  # Persistence

  - Implement serialization for `Exp`.

  - Implement persistance API

    - load :: IO (Map Text ByteString)
    - fech :: Text -> IO (Maybe ByteString)
    - stor :: Text -> ByteString -> IO ()

  - Implement Unix persistence driver.

    - Files go to ~/.uruk/raw/
    - `=x K` becomes `writeFile "~/.uruk/raw/x" (dump k)`
    - `!x` becomes `unlinkFile "~/.uruk/raw/x"`

  - Implement Javascript persistence driver.

  # REPL

  - Parse decl or undecl or expression.

  - if decl
    - evaluate
    - print
    - add to environment
    - write to disk
  - if undecl
    - remove from environment
    - remove from disk
  - otherwise
    - evaluate
    - print

  # Dash

  - Make bindings available for display.

  - Make last execution trace available for display.

  - What do I want to display in the UI?

    - Current list of bindings.

      - Data associated with each binding.

      - List of commands, their results, and their execution traces.
-}

module Urbit.Uruk.UrukDemo where

import ClassyPrelude hiding (exp, init, last, many, some, try)

import Control.Monad.State.Lazy
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Tree

import Control.Arrow    ((>>>))
import Data.Bits        (shiftL, (.|.))
import Data.Function    ((&))
import Data.Void        (Void)
import Numeric.Natural  (Natural)


-- Types -----------------------------------------------------------------------

infixl 5 :@;

data Ur = J | K | S | D | V Text deriving (Eq, Ord)

data Exp = N Ur | Exp :@ Exp deriving (Eq, Ord)

data Dec = Dec Text Exp
  deriving (Eq, Ord, Show)

type Env = Map Text Exp

-- type Exec a = ExecptT Text (StateT Env IO a)


-- Instances -------------------------------------------------------------------

instance Show Ur where
  show = \case { J→"J"; K→"K"; S→"S"; D→"D"; V v→unpack v }

tree ∷ Exp → Tree Ur
tree = go [] where go a = \case { N n → Node n a; x :@ y → go (tree y:a) x }

unTree ∷ Tree Ur → Exp
unTree (Node n xs) = foldl' (:@) (N n) (unTree <$> xs)

showTree ∷ Tree Ur -> String
showTree (Node n []) = show n
showTree (Node n xs) = "(" <> intercalate " " (show n : fmap showTree xs) <> ")"

instance Show Exp where show = showTree . tree

eval ∷ (Text → Maybe Exp) → Exp → Exp
eval env = \case { (reduce env → Just x) → eval env x; x → x }

exec ∷ (Text → Maybe Exp) → Exp → [Exp]
exec env x = x : fromMaybe [] (fmap (exec env) (reduce env x))

reduce ∷ (Text → Maybe Exp) → Exp → Maybe Exp
reduce env = go
 where
  go = \case
    N (V x)               → env x
    N K :@ x :@ _         → Just x
    (go→Just xv) :@ y     → Just (xv :@ y)
    x :@ (go→Just yv)     → Just (x :@ yv)
    N S :@ x :@ y :@ z    → Just (x :@ z :@ (y :@ z))
    N D :@ x              → Just (jam x)
    (jetRule→Just(b,xs))  → Just (foldl' (:@) b xs)
    _                     → Nothing

jetRule ∷ Exp → Maybe (Exp, [Exp])
jetRule x = do
  (n, rest) ← jetHead (tree x)
  (b, xs)   ← case rest of { _:b:xs → Just (b,xs); _ → Nothing }
  guard (fromIntegral n == length xs)
  Just (unTree b, unTree <$> xs)

jetHead :: Tree Ur -> Maybe (Natural, [Tree Ur])
jetHead = \(Node n xs) -> guard (n == J) $> go 1 xs
 where
  go n (Node J [] : xs) = go (succ n) xs
  go n xs               = (n, xs)

jam ∷ Exp → Exp
jam = (N J :@ N J :@ N K :@) . enc . snd . go
 where
  enc 0 = N S :@ N K
  enc n = N S :@ (N S :@ (N K :@ N S) :@ N K) :@ enc (pred n)

  urEnum :: Ur -> Natural
  urEnum J     = 0
  urEnum K     = 1
  urEnum S     = 2
  urEnum D     = 3
  urEnum (V v) = error ("undefined variable" <> unpack v)

  go (N n)    = (3, urEnum n*2)
  go (x :@ y) = (rBits ∷ Int, rNum ∷ Natural)
   where
    ((xBits, xNum), (yBits, yNum)) = (go x, go y)
    rBits = 1 + xBits + yBits
    rNum  = 1 .|. shiftL xNum 1 .|. shiftL yNum (1 + xBits)


-- Parsing Types ---------------------------------------------------------------

data Mode = Wide | Tall

type Parser = StateT Mode (Parsec Void Text)

inWideMode :: Parser a -> Parser a
inWideMode = withLocalState Wide
 where
  withLocalState :: Monad m => s -> StateT s m a -> StateT s m a
  withLocalState val x = do
    old <- get
    put val
    x <* put old

-- Simple Lexers ---------------------------------------------------------------

ace, pal, par :: Parser ()
ace = void (char ' ')
pal = void (char '(')
par = void (char ')')

bulkSpace :: Parser ()
bulkSpace = void (many spaceChar)

gap :: Parser ()
gap = (void (string "  ") <|> void (char '\n')) >> bulkSpace

whitespace :: Parser ()
whitespace = gap <|> ace

sym :: Parser Text
sym = fmap pack $ some $ oneOf ("$" <> ['a' .. 'z'] <> ['A' .. 'Z'])

bind :: Parser Text
bind = fmap pack $ some $ oneOf ("$" <> ['a' .. 'z'])


-- Grammar ---------------------------------------------------------------------

exp :: Parser Exp
exp = try rune <|> irregular

apN :: [Exp] -> Exp
apN []       = error "empty function application"
apN [x     ] = x
apN (x : xs) = go x xs
 where
  go acc []       = acc
  go acc (y : ys) = go (acc :@ y) ys

ap3 :: Exp -> Exp -> Exp -> Exp
ap3 x y z = apN [x, y, z]

ap4 :: Exp -> Exp -> Exp -> Exp -> Exp
ap4 x y z p = apN [x, y, z, p]

ur :: Parser Ur
ur = choice
  [ S <$ char 'S'
  , K <$ char 'K'
  , J <$ char 'J'
  , D <$ char 'D'
  , V <$> sym
  ]

irregular :: Parser Exp
irregular = inWideMode (call <|> N <$> ur)
  where call = apN <$> grouped "(" " " ")" exp

sig :: Parser [Text]
sig = grouped "(" " " ")" sym <|> (: []) <$> sym

rune :: Parser Exp
rune = choice
  [ string "%-" *> rune2 (:@) exp exp
  , string "%+" *> rune3 ap3 exp exp exp
  , string "%^" *> rune4 ap4 exp exp exp exp
  ]


-- Groups and binders ----------------------------------------------------------

grouped :: Text -> Text -> Text -> Parser a -> Parser [a]
grouped open delim close item = string open >> body
 where
  body = shut <|> (:) <$> item <*> rest
  rest = many (string delim *> item) <* shut
  shut = string close $> []


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


-- Entry Point -----------------------------------------------------------------

decl :: Parser Dec
decl = do
  char '='
  n <- bind
  ace
  b <- exp
  pure (Dec n b)

eat :: Parser ()
eat = option () whitespace

eaten :: Parser a -> Parser a
eaten x = eat *> x <* eat

file :: Parser a -> Parser a
file x = eaten x <* eof

decls :: Parser [Dec]
decls = (:) <$> decl <*> many (try (gap *> decl))

expFile :: Parser Exp
expFile = file exp

dashFile :: Parser [Dec]
dashFile = file decls

doParse :: Parser a -> Text -> Either Text a
doParse act txt =
  runParser (evalStateT act Tall) "stdin" txt & \case
    Left  e → Left (pack $ errorBundlePretty e)
    Right x → pure x

parseExp ∷ Text → Either Text Exp
parseExp = doParse (file exp)

parseDecl ∷ Text → Either Text Dec
parseDecl = doParse (file decl)

parseDecls ∷ Text → Either Text [Dec]
parseDecls = doParse (file decls)

declExp ∷ Parser (Dec, Exp)
declExp = do
  d <- decl
  gap
  b <- exp
  pure (d, b)


--------------------------------------------------------------------------------

globals :: Env
globals = mempty

tryExp :: Text -> IO Exp
tryExp = parseExp >>> \case
  Left err -> putStrLn err >> error ""
  Right x  -> pure x

tryDecl :: Text -> IO (Text, Exp)
tryDecl = parseDecl >>> \case
  Left err        -> error (unpack err)
  Right (Dec n v) -> do
    putStrLn (n <> "=")
    res <- tryExec (`lookup` globals) v
    pure (n, res)

tryExec :: (Text -> Maybe Exp) -> Exp -> IO Exp
tryExec env = exec env >>> go
 where
  go []     = error "impossible"
  go [x]    = print x >> pure x
  go (x:xs) = print x >> go xs

tryExecBind :: (Text -> Maybe Exp) -> Text -> Exp -> IO Exp
tryExecBind env nm = exec env >>> go
 where
  go []     = error "impossible"
  go [x]    = out x >> pure x
  go (x:xs) = out x >> go xs

  out x = putStrLn ("=" <> nm <> " " <> tshow x)

noFree :: Exp -> IO Exp
noFree = \case
  N (V v) -> error ("undefined variable: " <> unpack v)
  N x       -> pure (N x)
  x :@ y    -> (:@) <$> noFree x <*> noFree y

tryDeclExp :: Text -> IO Exp
tryDeclExp txt = doParse (file declExp) txt & \case
  Left  err             -> error (unpack err)
  Right (Dec n v, body) -> do
    dvl <- tryExecBind (`lookup` globals) n v
    noFree dvl
    putStrLn ""
    let env = insertMap n dvl globals
    tryExec (`lookup` env) body

main :: IO ()
main = do
  [txt] <- getArgs
  void (tryDeclExp txt)
