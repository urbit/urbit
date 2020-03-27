{-- OPTIONS_GHC -Wall -Werror --}

module Urbit.Uruk.DashParser where

import ClassyPrelude hiding (exp, init, last, many, some, try)

import Control.Lens
import Control.Monad.State.Lazy
import Text.Megaparsec
import Text.Megaparsec.Char

import Bound            (abstract1)
import Data.Void        (Void, absurd)
import Numeric.Natural  (Natural)
import Prelude          (read, last)
import Text.Show.Pretty (pPrint)
import Urbit.Atom       (Atom, atomUtf8Exn)

import qualified Urbit.Atom          as Atom
import qualified Urbit.Uruk.Bracket  as B
import qualified Urbit.Uruk.UrukDemo as UD


-- Types -----------------------------------------------------------------------

infixl 5 :@;

data AST
  = Lam Text AST
  | Var Text
  | AST :@ AST
  | Tag Text
 deriving (Eq, Ord, Show)

pattern App :: AST -> AST -> AST
pattern App x y = x :@ y


-- Parser Monad ----------------------------------------------------------------

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
sym =
  fmap pack $ some $ oneOf ("$" <> ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'])


-- Grammar ---------------------------------------------------------------------

exp :: Parser AST
exp = try rune <|> irregular

apN :: [AST] -> AST
apN []       = error "empty function application"
apN [x     ] = x
apN (x : xs) = go x xs
 where
  go acc []       = acc
  go acc (y : ys) = go (acc :@ y) ys

ap3 :: AST -> AST -> AST -> AST
ap3 x y z = apN [x, y, z]

ap4 :: AST -> AST -> AST -> AST -> AST
ap4 x y z p = apN [x, y, z, p]

lam :: [Text] -> AST -> AST
lam binds body = go body binds
 where
  go acc []       = acc
  go acc (b : bs) = Lam b (go acc bs)

irregular :: Parser AST
irregular = inWideMode $ choice
  [ inlineFn
  , apN <$> grouped "(" " " ")" exp
  , Var <$> sym
  , Tag <$> try (char '%' >> sym)
  ]

inlineFn :: Parser AST
inlineFn = do
  char '<'
  binds <- some (try (sym <* char ' '))
  body  <- exp
  char '>'
  pure (lam binds body)

sig :: Parser [Text]
sig = grouped "(" " " ")" sym <|> (: []) <$> sym

rune :: Parser AST
rune = choice
  [ string "|=" *> rune2 lam sig exp
  , string "%-" *> rune2 (:@) exp exp
  , string "%+" *> rune3 ap3 exp exp exp
  , string "%^" *> rune4 ap4 exp exp exp exp
  , string "~/" *> rune3 jet nat sym exp
  ]

jet :: Natural -> Text -> AST -> AST
jet arity name expr = go j arity :@ Tag name :@ expr
 where
  j = Var "J"

  go :: AST -> Natural -> AST
  go _   0 = error "jet: go: bad-arity: 0"
  go acc 1 = acc
  go acc n = go (acc :@ j) (pred n)

nat :: Parser Natural
nat = read <$> some (oneOf ['0' .. '9'])

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

data Dec = Dec Text [Text] AST
  deriving (Show)

decl :: Parser Dec
decl = do
  (n:xs, b) <- string "++" *> rune2 (,) sig exp
  pure (Dec n xs b)

eat :: Parser ()
eat = option () whitespace

dashFile :: Parser [Dec]
dashFile = go []
 where
  go acc = do
    eat
    (eof $> reverse acc <|> (decl >>= go . (: acc)))

parseAST ∷ Text → Either Text AST
parseAST txt =
  runParser (evalStateT exp Tall) "stdin" txt & \case
    Left  e → Left (pack $ errorBundlePretty e)
    Right x → pure x

parseDecs ∷ Text → Either Text [Dec]
parseDecs txt =
  runParser (evalStateT dashFile Tall) "stdin" txt & \case
    Left  e → Left (pack $ errorBundlePretty e)
    Right x → pure x


-- AST to SK -------------------------------------------------------------------

infixl 5 :%;

data SKAG
  = S
  | K
  | A Atom
  | SKAG :% SKAG
 deriving (Show)

skag :: B.Exp Void (B.SK Atom) -> SKAG
skag = \case
  B.Lam b    _  -> absurd b
  x     B.:@ y  -> skag x :% skag y
  B.Var (B.V a) -> A a
  B.Var B.S     -> S
  B.Var B.K     -> K

astExp :: AST -> B.Exp () (Either Atom Text)
astExp = \case
  Var t   -> B.Var $ Right t
  Tag t   -> B.Var $ Left $ Atom.utf8Atom t
  x :@ y  -> astExp x B.:@ astExp y
  Lam n b -> B.Lam () (abstract1 (Right n) (astExp b))

decExp :: Dec -> (Text, B.Exp () (Either Atom Text))
decExp (Dec nm jetArgs e) = (nm, body)
 where
  body = fromIntegral (length jetArgs) & \case
    0 -> astExp e
    n -> astExp (jet n nm (lam jetArgs e))

expErr :: B.Exp b (Either Atom Text) -> Either Text (B.Exp b Atom)
expErr = traverse $ \case
  Left atom  -> Right atom
  Right free -> Left ("Undefined Variable: " <> free)

tryExp ∷ Text → Either Text (SKAG, SKAG)
tryExp txt = do
  expr <- parseAST txt
  resu <- expErr (astExp expr)
  pure (skag (B.johnTrompBracket resu), skag (B.naiveBracket resu))


-- Texting ---------------------------------------------------------------------

tryIt :: Text -> IO ()
tryIt txt = do
  tryExp txt & \case
    Left err -> putStrLn err
    Right rs -> pPrint rs

prettyThing :: Text -> B.Out (B.SK (Either Atom Text)) -> IO ()
prettyThing nm topExpr = do
  putStrLn ("++  " <> nm)
  putStr "  "
  putStr (go topExpr)
  putStrLn ""
 where
  go = \case
    B.Lam v _            -> absurd v
    B.Var B.S            -> "S"
    B.Var B.K            -> "K"
    B.Var (B.V (Left a)) -> Atom.atomUtf8 a & \case
      Left  _ -> tshow a
      Right t -> "%" <> t
    B.Var (B.V (Right t)) -> t
    x B.:@ y -> "(" <> intercalate " " (go <$> flatten x [y]) <> ")"

  flatten (x B.:@ y) acc = flatten x (y : acc)
  flatten x          acc = (x : acc)

tryDash :: IO ()
tryDash = do
  txt <- readFileUtf8 "urbit-uruk/jets.dash"
  parseDecs txt & \case
    Left err -> putStrLn err
    Right ds -> do
      resolv (fmap B.johnTrompBracket . decExp <$> ds) & \case
        Left err -> putStrLn err
        Right ev -> pPrint (mapToList ev)

type Env = Map Text UD.Exp

resolv :: [(Text, B.Out (B.SK (Either Atom Text)))] -> Either Text Env
resolv = go initialEnv
 where
  initialEnv :: Env
  initialEnv = mapFromList
    [("S", UD.N UD.S), ("K", UD.N UD.K), ("J", UD.N UD.J), ("D", UD.N UD.D)]

  go :: Env -> [(Text, B.Out (B.SK (Either Atom Text)))] -> Either Text Env
  go acc []            = pure acc
  go acc ((n, e) : xs) = do
    --  traceM (unpack n)
    e' <- cvt n acc e
    --  traceM (show e')
    e'' <- case udEval e' of
                UD.N n -> pure (UD.N n)
                x UD.:@ y -> pure (x UD.:@ y)
    --  traceM (show e'')
    go (insertMap n e'' acc) xs

udEval :: UD.Exp -> UD.Exp
udEval = last . UD.exec (pure Nothing)

cvt :: Text -> Env -> B.Out (B.SK (Either Atom Text)) -> Either Text UD.Exp
cvt bind env = go
 where
  go = \case
    B.Lam v _               -> absurd v
    B.Var B.S               -> pure $ UD.N $ UD.S
    B.Var B.K               -> pure $ UD.N $ UD.K
    B.Var (B.V (Left atom)) -> pure $ UD.N $ UD.V ("\"" <> atomUtf8Exn atom <> "\"")
    x B.:@ y                -> (UD.:@) <$> go x <*> go y
    B.Var (B.V (Right txt)) -> lookup txt env & \case
      Nothing -> Left ("Undefined variable: " <> txt <> " (in " <> bind <> ")")
      Just v  -> pure v
