{-# OPTIONS_GHC -Wall -Werror #-}
{-# OPTIONS_GHC -Werror #-}

{-
  DONE Make REPL flow available to caller.
  TODO Implement an actual REPL with bindings.
  TODO Add persistance to REPL.
  TODO Add persistance to REPL.

  # Persistence

  TODO Serialization for `Exp`.
  TODO Unix persistance.
  TODO Web (Local Storage) persistance.
-}

module Urbit.Uruk.UrukDemo
  ( main
  , Exp
  , Env
  , Inp(..)
  , EvalResult(..)
  , execInp
  , InpResult(..)
  , parseInps
  , execText
  , printEvalResult
  , printInpResult
  , prettyInpResult
  )
where

import ClassyPrelude hiding (exp, init, last, many, some, try)

import Control.Monad.State.Lazy
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Tree
import Codec.Serialise (Serialise)

import Data.Bits       (shiftL, (.|.))
import Data.Function   ((&))
import Data.Void       (Void)
import Numeric.Natural (Natural)


-- Types -----------------------------------------------------------------------

infixl 5 :@;

data Ur = J | K | S | D | V Text deriving (Eq, Ord)
  deriving (Generic)
  deriving anyclass (NFData, Serialise)

data Exp = N Ur | Exp :@ Exp deriving (Eq, Ord)
  deriving (Generic)
  deriving anyclass (NFData, Serialise)

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

exec ∷ (Text → Maybe Exp) → Exp → [Exp]
exec env = go
 where
  go :: Exp -> [Exp]
  go x = (maybe (goNm x) (x:) . fmap go . reduce env) x

  goNm :: Exp -> [Exp]
  goNm x = (maybe [x] (x:) . fmap goNm . reduceVars env) x

reduce ∷ (Text → Maybe Exp) → Exp → Maybe Exp
reduce env = go
 where
  go = \case
    N (V x) :@ r          → (:@) <$> env x <*> pure r
    N K :@ x :@ _         → Just x
    (go→Just xv) :@ y     → Just (xv :@ y)
    x :@ (go→Just yv)     → Just (x :@ yv)
    N S :@ x :@ y :@ z    → Just (x :@ z :@ (y :@ z))
    N D :@ x              → Just (jam x)
    (jetRule→Just(b,xs))  → Just (foldl' (:@) b xs)
    _                     → Nothing

reduceVars ∷ (Text → Maybe Exp) → Exp → Maybe Exp
reduceVars env = go
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
sym = fmap pack $ some $ oneOf
  ("$-%" <> ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'])

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

rune :: Parser Exp
rune = choice
  [ string "%." *> rune2 (flip (:@)) exp exp
  , string "%-" *> rune2 (:@) exp exp
  , string "%+" *> rune3 ap3 exp exp exp
  , string "%^" *> rune4 ap4 exp exp exp exp
  , string "%*" *> runeN apN exp
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
runeN node x = node <$> parseRune tall wide
  where tall = gap >> elems
                 where elems   = term <|> elemAnd
                       elemAnd = do v ← x; gap; vs ← elems; pure (v:vs)
                       term    = string "==" $> []
        wide = pal *> option [] elems <* par
                 where elems = (:) <$> x <*> many (ace >> x)


-- Entry Point -----------------------------------------------------------------

decl :: Parser Dec
decl = do
  char '='
  n <- bind
  ace
  b <- exp
  pure (Dec n b)

unbind :: Parser Text
unbind = char '!' *> bind

data Inp
  = Decl Dec
  | Expr Exp
  | Wipe Text
 deriving (Eq, Ord, Show)

inp :: Parser Inp
inp = choice [ Decl <$> decl, Expr <$> exp, Wipe <$> unbind ]

eat :: Parser ()
eat = option () whitespace

eaten :: Parser a -> Parser a
eaten x = eat *> x <* eat

file :: Parser a -> Parser a
file x = eaten x <* eof

inps :: Parser [Inp]
inps = (:) <$> inp <*> many (try (gap *> inp))

doParse :: Parser a -> Text -> Either Text a
doParse act txt =
  runParser (evalStateT act Tall) "stdin" txt & \case
    Left  e → Left (pack $ errorBundlePretty e)
    Right x → pure x


--------------------------------------------------------------------------------

noFree :: Env -> Exp -> Either Text Exp
noFree env = \case
  N (V ((`lookup` env) -> Just x)) -> noFree env x
  N (V v                         ) -> Left ("undefined variable: " <> v)
  N x                              -> pure (N x)
  x :@ y                           -> (:@) <$> noFree env x <*> noFree env y

tryInps :: Env -> Text -> IO Env
tryInps initEnv txt = doParse (file inps) txt & \case
  Left  err -> error (unpack err)
  Right ips -> go initEnv ips
 where
  go :: Env -> [Inp] -> IO Env
  go env []       = pure env
  go env (e : es) = do
    case execInp env e of
      Left err -> error (unpack err)
      Right (env', res) -> do
        printInpResult res
        go env' es

printInpResult :: InpResult -> IO ()
printInpResult = \case
  InpWipe v     -> putStrLn ("!" <> v)
  InpExpr _ r   -> printEvalResult Nothing r
  InpDecl v _ r -> printEvalResult (Just v) r

prettyInpResult :: InpResult -> (Text, Text)
prettyInpResult = \case
  InpWipe v     -> ("!" <> v, "")
  InpExpr _ r   -> prettyEvalResult Nothing r
  InpDecl v _ r -> prettyEvalResult (Just v) r

prettyEvalResult :: Maybe Text -> EvalResult -> (Text, Text)
prettyEvalResult mTxt (EvalResult val trac) =
  (resultStr, unlines $ reverse traceStrs)
 where
  resultStr = case mTxt of
    Nothing -> tshow val
    Just nm -> "=" <> nm <> " " <> tshow val

  traceStrs = case mTxt of
    Nothing -> trac <&> \x -> ".. " <> tshow x
    Just nm -> trac <&> \x -> "=" <> nm <> " " <> tshow x

printEvalResult :: Maybe Text -> EvalResult -> IO ()
printEvalResult mTxt (EvalResult val trac) = do
  for_ trac $ \x -> do
    case mTxt of
      Nothing -> putStrLn (".. " <> tshow x)
      Just nm -> putStrLn ("=" <> nm <> " " <> tshow x)

  case mTxt of
    Nothing -> print val
    Just nm -> putStrLn ("=" <> nm <> " " <> tshow val)

main :: IO ()
main = do
  void $ pure $ execInp mempty (Wipe "K")
  [txt] <- getArgs
  void (tryInps mempty txt)

--------------------------------------------------------------------------------

data EvalResult = EvalResult { erExp :: Exp, erTrace :: [Exp] }
 deriving (Eq, Ord, Show)

data InpResult
  = InpWipe Text
  | InpExpr Exp EvalResult
  | InpDecl Text Exp EvalResult
 deriving (Eq, Ord, Show)

doEval :: Env -> Exp -> Either Text EvalResult
doEval e v = do
  let trac = exec (`lookup` e) v
  res <- lastEit trac
  pure (EvalResult res trac)
 where
  lastEit []     = Left "doEval: empty trace (This should not be possible)"
  lastEit [x]    = Right x
  lastEit (_:xs) = lastEit xs

execInp :: Env -> Inp -> Either Text (Env, InpResult)
execInp env = \case
  Wipe v -> Right (deleteMap v env, InpWipe v)
  Expr x -> do
    res <- doEval env x
    let val = case noFree env (erExp res) of
                Left _  -> erExp res
                Right v -> v
    pure (env, InpExpr x res{erExp=val})
  Decl (Dec v x) -> do
    res <- doEval env x
    val <- noFree env (erExp res)
    pure (insertMap v (erExp res) env, InpDecl v x res{erExp=val})

parseInps :: Text -> Either Text [Inp]
parseInps = doParse (file inps)

execText :: Env -> Text -> Either Text (Env, [InpResult])
execText initEnv txt = do
  ips <- doParse (file inps) txt
  go initEnv [] ips
 where
  go :: Env -> [InpResult] -> [Inp] -> Either Text (Env, [InpResult])
  go env acc []       = pure (env, reverse acc)
  go env acc (e : es) = do
    (env', res) <- execInp env e
    go env' (res : acc) es
