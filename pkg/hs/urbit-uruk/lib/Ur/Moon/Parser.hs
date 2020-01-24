-- TODO Handle comments

module Ur.Moon.Parser where

import ClassyPrelude            hiding (exp, head, many, some, try, pair)
import Control.Monad.State.Lazy
import Text.Megaparsec          hiding (parse)
import Ur.Moon.Lexer            hiding (Nat, Parser)

import Control.Arrow ((>>>))
import Control.Lens  ((&))
import Data.List     (elemIndex)
import Data.Void     (Void)
import GHC.Natural   (Natural)
import Prelude       (head)

import qualified Ur.Moloch as Mo


-- AST -------------------------------------------------------------------------

type Nat = Natural

data Tie = Tie
    { tieName :: Maybe Text
    , tieType :: Exp
    }
  deriving (Eq, Ord)

data Exp
    = Var Text
    | App Exp Exp
    | Lam Tie Exp
    | Pie Tie Exp
    | Con Exp Exp
    | Box Exp
    | Uni
    | Lit Nat
  deriving (Eq, Ord)

instance Show Exp where
    show = \case
        Var t       → unpack t
        Lit n       → show n
        Uni         → "[]"
        Box x       → "[" <> show x <> "]"
        e@(App _ _) → showList (getApps [] e) "(" ")"
        e@(Lam _ _) → showList (getLams [] e) "<" ">"
        e@(Pie _ _) → showList (getPies [] e) "{" "}"
        e@(Con _ _) → showList (getCons [] e) "[" "]"
      where
        getApps acc (App x y) = getApps (y:acc) x
        getApps acc e         = (e:acc)

        getPies acc (Pie x y) = getPies (x:acc) y
        getPies acc e         = reverse (Tie Nothing e:acc)

        getLams acc (Lam x y) = getLams (x:acc) y
        getLams acc e         = reverse (Tie Nothing e:acc)

        getCons acc (Con x y) = getCons (x:acc) y
        getCons acc e         = reverse (e:acc)

        showList xs pre post = pre <> (intercalate " " (show <$> xs)) <> post

instance Show Tie where
    show (Tie Nothing typ)   = show typ
    show (Tie (Just nm) typ) = unpack nm <> "/" <> show typ


-- Parser Monad ----------------------------------------------------------------

data Mode = Wide | Tall
  deriving (Eq, Ord, Show)

withLocalState :: Monad m => s -> StateT s m a -> StateT s m a
withLocalState val x = do { old <- get; put val; x <* put old }

inWideMode :: Parser a -> Parser a
inWideMode = withLocalState Wide

type Parser = StateT Mode (Parsec Void Text)


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
                       term    = end $> []
        wide = pal *> option [] elems <* par
                 where elems = (:) <$> elem <*> many (ace >> elem)


-- Runes -----------------------------------------------------------------------

call ∷ Parser Exp
call = inWideMode (pal *> body <* par)
  where
    body = apps <$> exp <*> some (ace *> exp)
    apps f [] = f
    apps f (x:xs) = apps (App f x) xs

lamb ∷ Parser Exp
lamb = inWideMode (gal *> body <* gar)
  where
    body = cloz <$> many (try (tie <* ace)) <*> exp
    cloz []     b = b
    cloz [x]    b = Lam x b
    cloz (x:xs) b = Lam x (cloz xs b)

pars ∷ Parser Exp
pars = inWideMode (lak *> (unit <|> (body <* rak)))
  where
    unit = rak >> pure (mk [])
    body = mk <$> ((:) <$> exp <*> many (ace *> exp))
    mk []     = Uni
    mk [x]    = Box x
    mk [x,y]  = Con x y
    mk (x:xs) = Con x (mk xs)

pies ∷ Parser Exp
pies = inWideMode (lob *> body <* rob)
  where
    body = do
        args <- many (try (tie <* ace))
        body <- exp
        pure (cloz args body)
    cloz []     b = b
    cloz [x]    b = Pie x b
    cloz (x:xs) b = Pie x (cloz xs b)

coreExp ∷ Parser Exp
coreExp = choice
    [ Var <$> idn
    , Lit <$> num
    , call
    , lamb
    , pies
    , pars
    ]

runes ∷ Parser Exp
runes = choice
    [ lEt >> rune3 mkLet tie exp exp
    , lam >> rune2 mkLam tie exp
    , ap2 >> rune2 mkAp2 exp exp
    , ap3 >> rune3 mkAp3 exp exp exp
    , ap4 >> rune4 mkAp4 exp exp exp exp
    , apn >> runeN mkApN exp
    , pi2 >> rune2 mkPi2 tie tie
    , pi3 >> rune3 mkPi3 tie tie tie
    , pi4 >> rune4 mkPi4 tie tie tie tie
    , pin >> runeN mkPiN tie
    ]
  where
    mkLet t v b    = App (Lam t b) v
    mkLam t b      = Lam t b

    mkApN []       = error "empty apply"
    mkApN [x]      = error "no arguments"
    mkApN [x,y]    = App x y
    mkApN [x,y,z]  = App (App x y) z
    mkApN (x:y:zs) = App (App x y) (mkApN zs)
    mkAp2 x y      = mkApN [x, y]
    mkAp3 x y z    = mkApN [x, y, z]
    mkAp4 x y z p  = mkApN [x, y, z, p]

    mkPiN []       = error "empty function type"
    mkPiN [x]      = error "function type with no arguments"
    mkPiN [x,y]    = Pie x (tieType y)
    mkPiN (x:xs)   = Pie x (mkPiN xs)
    mkPi2 x y      = mkPiN [x, y]
    mkPi3 x y z    = mkPiN [x, y, z]
    mkPi4 x y z p  = mkPiN [x, y, z, p]

exp :: Parser Exp
exp = runes <|> coreExp

tie :: Parser Tie
tie = inWideMode (try withName <|> without)
  where
    withName = Tie <$> (Just <$> idn) <*> (fas >> exp)
    without  = Tie Nothing <$> exp

whitespace = void $ many $ ace <|> gap

-- Entry Point -----------------------------------------------------------------

moonFile :: Parser Exp
moonFile = do
  option () whitespace
  e ← exp
  option () whitespace
  eof
  pure e

parse :: Text -> Either Text Exp
parse txt =
  runParser (evalStateT moonFile Tall) "stdin" txt & \case
    Left  e -> Left (pack $ errorBundlePretty e)
    Right x -> pure x

tryParse :: Text -> IO ()
tryParse txt = parse txt & \case
    Left err  -> putStrLn err
    Right res -> do
        let mo = comp res
        let mo' = Mo.normalize mo
        print mo
        print mo'


-- Main ------------------------------------------------------------------------

parseExpTest ∷ Text → IO ()
parseExpTest = parseTest (evalStateT (exp <* eof) Tall)

main ∷ IO ()
main = (head <$> getArgs) >>= parseExpTest


-- Compiler --------------------------------------------------------------------

data Env = Env
  { ePrims :: Map Text Mo.Exp
  , eBinds :: [Text]
  }

envLookup :: Env -> Text -> Either Text Mo.Exp
envLookup Env{..} txt =
    maybe (Left ("Unbound variable: " <> txt)) pure $ asum
        [ Mo.Var . fromIntegral <$> elemIndex txt eBinds
        , lookup txt ePrims
        ]

insEnv :: Env -> Text -> Env
insEnv (Env p b) n = Env p (n:b)

comp :: Exp -> Mo.Exp
comp = compEnv (Env prims []) >>> \case
    Left msg -> error (unpack msg)
    Right ex -> ex

compEnv :: Env -> Exp -> Either Text Mo.Exp
compEnv = go
  where
    go e = \case
        Var n   -> envLookup e n
        App x y -> Mo.App <$> go e x <*> go e y
        Lit n   -> pure (Mo.NatLit n)
        Uni     -> pure (Mo.UnitLit)
        Box x   -> Mo.JetLit <$> go e x
        Con x y -> Mo.PairLit <$> go e x <*> go e y
        Lam t b -> case t of
                     Tie mNm ty → do
                       Mo.Lam <$> go e ty <*> go (insEnv e (fromMaybe "" mNm)) b
        Pie t b -> case t of
                     Tie mNm ty →
                       Mo.Pie <$> go e ty <*> go (insEnv e (fromMaybe "" mNm)) b

prims ∷ Map Text Mo.Exp
prims = mapFromList
    [ ("@",        n)
    , ("*",        t)
    , ("_",        b)
    , ("inc",      inc)
    , ("fold-nat", fol)
    , ("Jet",      jet)
    , ("fast",     jit)
    , ("fire",     fir)
    , ("eval",     evl)
    , ("dump",     dum)
    ]
  where
    v = Mo.Var
    l = Mo.Lam
    n = Mo.Nat
    t = Mo.Cns Mo.Tar
    b = Mo.Cns Mo.Box
    j = Mo.Jet

    {-
        TODO Implement `dump`.
        TODO Switch to the box/unbox/fire semantics of MicroMoloch.
        TODO JetLit probably needs type parameters?
        TODO JetApp probably needs type parameters?
    -}
    inc = l n $ Mo.NatInc (v 0)
    fol = l n $ Mo.NatDec (v 0)
    evl = l t $ l n     $ Mo.Eval   (v 1) (v 0)
    dum = evl
    jet = l t $ l t     $ Mo.Jet    (v 1) (v 0)
    jit = l t $ l (v 0) $ Mo.JetLit (v 0)
    fir = l t $ l t $ l (j (v 1) (v 0)) $ l (v 1) $ Mo.JetApp (v 1) (v 0)
