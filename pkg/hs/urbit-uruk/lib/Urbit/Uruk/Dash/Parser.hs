{-- OPTIONS_GHC -Wall -Werror #-}

module Urbit.Uruk.Dash.Parser where

import ClassyPrelude hiding (exp, init, many, some, try, elem, Prim)

import Control.Lens hiding (snoc)
import Control.Monad.State.Lazy
import Text.Megaparsec hiding (Pos)
import Text.Megaparsec.Char
import Data.Tree
import Urbit.Pos
import Urbit.Moon.Arity
import Urbit.Uruk.Dash.Exp

import Bound                 (abstract1, toScope, fromScope)
import Bound.Var             (Var(..))
import Data.Void             (Void, absurd)
import Numeric.Natural       (Natural)
import Prelude               (read)
import Text.Show.Pretty      (pPrint, ppShow)
import Urbit.Atom            (Atom)
import Urbit.Uruk.JetSpec    (jetSpec)
import Urbit.Moon.MakeStrict (makeStrict, Prim(..))
import qualified Data.Char           as C
import qualified Language.Haskell.TH as TH
import qualified Urbit.Atom          as Atom
import qualified Urbit.Moon.Bracket  as B


-- Numbers ---------------------------------------------------------------------

type Nat = Natural


-- Syntax Tree -----------------------------------------------------------------

infixl 5 :@;

data AST
  = Lam Text AST
  | Var Text
  | AST :@ AST
  | Tag Text
 deriving (Eq, Ord, Show)

data Dec = Dec Text [Text] AST
  deriving (Show)


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


-- Dashboard Processing --------------------------------------------------------

type Env = Map Text Exp

type Reg = Map SingJet (Pos, Val, Val)


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
  ("$-" <> ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'])


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
  , string "%*" *> runeN apN exp
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

runeN ∷ ([a]→b) → Parser a → Parser b
runeN node elem = node <$> parseRune tall wide
  where tall = gap >> elems
                 where elems   = term <|> elemAnd
                       elemAnd = do x ← elem; gap; xs ← elems; pure (x:xs)
                       term    = string "==" $> []
        wide = pal *> option [] elems <* par
                 where elems = (:) <$> elem <*> many (ace >> elem)


-- Entry Point -----------------------------------------------------------------

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

astExp :: AST -> B.Exp Atom () Text
astExp = \case
  Var t   -> B.Var t
  Tag t   -> B.Pri $ Atom.utf8Atom t
  x :@ y  -> astExp x B.:@ astExp y
  Lam n b -> B.Lam () (abstract1 n (astExp b))


-- Texting ---------------------------------------------------------------------

tryDash :: IO ()
tryDash = do
  txt <- readFileUtf8 "urbit-uruk/jets.dash"
  parseDecs txt & \case
    Left err -> putStrLn err
    Right ds -> do
      resolv ds & \case
        Left err -> putStrLn err
        Right (ev, rg) -> do
          pPrint (mapToList ev)
          pPrint (mapToList rg)

cvt :: Text -> Env -> B.Out (B.SK Atom) Text -> Either Text Exp
cvt bind env = go
 where
  go = \case
    B.Lam v _      -> absurd v
    B.Pri B.S      -> pure (N S)
    B.Pri B.K      -> pure (N K)
    B.Pri (B.P at) -> pure $ N $ DataJet $ NAT at
    x B.:@ y       -> (:&) <$> go x <*> go y
    B.Var txt      -> lookup txt env & \case
      Nothing -> Left ("Undefined variable: " <> txt <> " (in " <> bind <> ")")
      Just v  -> pure v

eval :: Exp -> Exp
eval x = maybe x eval (reduce x)

reduce :: Exp -> Maybe Exp
reduce = go
 where
  go :: Exp -> Maybe Exp
  go = \case
    N K :& x :& _             -> Just x
    (go -> Just xv) :& y      -> Just (xv :& y)
    x :& (go -> Just yv)      -> Just (x :& yv)
    N S :& x :& y :& z        -> Just (x :& z :& (y :& z))
    N D :& _                  -> Nothing
    (jetRule -> Just (b,xs))  -> Just (foldl' (:&) b xs)
    _                         -> Nothing

jetRule :: Exp -> Maybe (Exp, [Exp])
jetRule x = do
  (n, rest) <- jetHead (tree x)
  (b, xs  ) <- case rest of
    _ : b : xs -> Just (b, xs)
    _          -> Nothing
  guard (fromIntegral n == length xs)
  Just (unTree b, unTree <$> xs)

jetHead :: Tree Ur -> Maybe (Natural, [Tree Ur])
jetHead = \(Node n xs) -> guard (n == J) $> go 1 xs
 where
  go n (Node J [] : xs) = go (succ n) xs
  go n xs               = (n, xs)

jetExp :: Text -> Val -> Maybe (SingJet, (Pos, Val, Val))
jetExp nm vl = do
  sj <- readMay (toUpper nm)
  jv <- jetTup
  pure (sj, jv)
 where
  jetTup :: Maybe (Pos, Val, Val)
  jetTup = do
    (arity, args) <- jetHead (tree vl)
    (jname, body) <- case args of { [n, b] -> Just (n, b); _ -> Nothing }
    pure (fromIntegral arity, unTree jname, unTree body)

resolv :: [Dec] -> Either Text (Env, Reg)
resolv = go (initialEnv, mempty)
 where
  initialEnv :: Env
  initialEnv = mapFromList [("S", N S), ("K", N K), ("J", N J), ("D", N D)]

  go :: (Env, Reg) -> [Dec] -> Either Text (Env, Reg)
  go er         []            = pure er
  go (env, reg) (d : xs) = do
    (env', reg') <- one (env, reg) d
    go (env', reg') xs

  one :: (Env, Reg) -> Dec -> Either Text (Env, Reg)
  one (env, reg) (Dec n args ast) = do
    bodExp <- resolveNames n env (astExp (lam args ast))

    let eagExp = makeStrict (tup reg) (jetWrap n (length args) bodExp)

    -- traceM ""
    -- traceM "[input]"
    -- traceM (ppShow bodExp)
    -- traceM ""
    -- traceM "[eager]"
    -- traceM (ppShow eagExp)
    -- traceM ""

    fulExp <- Right $! force $ eval $ ski $ B.johnTrompBracket eagExp

    case jetExp n fulExp of
      Nothing -> do
        pure (insertMap n fulExp env, reg)
      Just (sj, (arity, nm, val)) -> do
        let env' = insertMap n (N $ SingJet sj) env
        let reg' = insertMap sj (arity, nm, val) reg
        pure (env', reg')

  ski :: B.Out (B.SK Exp) Void -> Exp
  ski = \case
    B.Lam b _     -> absurd b
    B.Var v       -> absurd v
    B.Pri B.S     -> N S
    B.Pri B.K     -> N K
    B.Pri (B.P v) -> v
    x B.:@ y      -> ski x :& ski y

  jetWrap :: Text -> Int -> B.Exp Exp () Void -> B.Exp Exp () Void
  jetWrap _  0       body = body
  jetWrap nm numArgs body =
    B.Pri (jetArityVal (fromIntegral numArgs)) B.:@ jetTagVal nm B.:@ body

-- jet :: Natural -> Text -> AST -> AST
-- jet arity name expr = go j arity :@ Tag name :@ expr
 -- where
  -- j = Var "J"
--
  -- go :: AST -> Natural -> AST
  -- go _   0 = error "jet: go: bad-arity: 0"
  -- go acc 1 = acc
  -- go acc n = go (acc :@ j) (pred n)

jetTagVal :: Text -> B.Exp Val v a
jetTagVal = B.Pri . N . DataJet . NAT . Atom.utf8Atom

jetArityVal :: Natural -> Val
jetArityVal = go (N J)
 where
  go _   0 = error "jetArity: go: bad arity: 0"
  go acc 1 = acc
  go acc n = go (acc :& N J) (n-1)

seqJet :: Exp
seqJet = N $ SingJet SEQ

yetJet :: Int -> Exp
yetJet = N . DataJet . In . fromIntegral

tup :: Reg -> Prim Exp -- (Exp, Int -> Exp, Exp, Exp -> Exp -> Exp, Exp -> Int)
tup reg = Prim{..}
 where
  pSeq = seqJet
  pYet = yetJet
  pKay = N K
  pApp = (:&)
  pArg = expArity reg

expArity :: Reg -> Exp -> Maybe Arity
expArity reg = go
 where
  go = \case
    N n    -> Just (urArity reg n)
    x :& y -> join (appArity <$> go x <*> go y)

urArity :: Reg -> Ur -> Arity
urArity _ S = AriEss
urArity _ K = AriKay
urArity _ J = AriJay 1
urArity _ D = AriDee
urArity _ (DataJet (NAT _)) = AriOth 2
urArity _ (DataJet (Sn  n)) = AriOth (2 + n)
urArity _ (DataJet (In  n)) = AriOth n
urArity _ (DataJet (Bn  n)) = AriOth (2 + n)
urArity _ (DataJet (Cn  n)) = AriOth (2 + n)
urArity r (SingJet sj     ) = case lookup sj r of
  Nothing        -> error ("Reference to undefined jet: " <> show sj)
  Just (n, _, _) -> AriOth n

resolveNames
  :: Text -> Env -> B.Exp Atom () Text -> Either Text (B.Exp Exp () Void)
resolveNames bind env = fmap (go Left) . traverse res
 where
  go :: (a -> Either Exp b) -> B.Exp Atom () a -> B.Exp Exp () b
  go f = \case
    B.Pri at     -> B.Pri $ N $ DataJet $ NAT at
    x     B.:@ y -> go f x B.:@ go f y
    B.Lam ()   x -> B.Lam () $ toScope $ go (wrap f) $ fromScope x
    B.Var v      -> f v & \case
      Left  e -> B.Pri e
      Right v -> B.Var v

  wrap :: (a -> Either Exp b) -> Var () a -> Either Exp (Var () b)
  wrap f (B ()) = Right (B ())
  wrap f (F fv) = F <$> f fv

  res :: Text -> Either Text Exp
  res = \nam -> case lookup nam env of
    Nothing -> Left ("Undefined variable: " <> nam <> " (in " <> bind <> ")")
    Just v  -> Right v

dashEnv :: Env
jetsMap :: Reg
(dashEnv, jetsMap) =
  either (error . unpack) id (parseDecs jetSpec >>= resolv)


-- Template Haskell ------------------------------------------------------------

intLit :: Integral i => i -> TH.Exp
intLit = TH.LitE . TH.IntegerL . fromIntegral

intLitPat :: Integral i => i -> TH.Pat
intLitPat = TH.LitP . TH.IntegerL . fromIntegral

sjExp :: SingJet -> TH.Exp
sjExp = TH.ConE . TH.mkName . show

djExp :: DataJet -> TH.Exp
djExp = \case
  Sn  p -> TH.ConE 'Sn `TH.AppE` intLit p
  In  p -> TH.ConE 'In `TH.AppE` intLit p
  Bn  p -> TH.ConE 'Bn `TH.AppE` intLit p
  Cn  p -> TH.ConE 'Cn `TH.AppE` intLit p
  NAT n -> TH.ConE 'NAT `TH.AppE` intLit n

urExp :: Ur -> TH.Exp
urExp = \case
  S          -> TH.ConE 'S
  K          -> TH.ConE 'K
  J          -> TH.ConE 'J
  D          -> TH.ConE 'D
  SingJet sj -> TH.ConE 'SingJet `TH.AppE` sjExp sj
  DataJet dj -> TH.ConE 'DataJet `TH.AppE` djExp dj

valExp :: Val -> TH.Exp
valExp = \case
  N n    -> TH.ConE 'N `TH.AppE` urExp n
  x :& y -> app `TH.AppE` valExp x `TH.AppE` valExp y
 where
  app = TH.ConE (TH.mkName ":&")

--------------------------------------------------------------------------------

djPat :: DataJet -> TH.Pat
djPat = \case
  Sn  p -> TH.ConP 'Sn [intLitPat p]
  In  p -> TH.ConP 'In [intLitPat p]
  Bn  p -> TH.ConP 'Bn [intLitPat p]
  Cn  p -> TH.ConP 'Cn [intLitPat p]
  NAT n -> TH.ConP 'NAT [intLitPat n]

sjName :: SingJet -> TH.Name
sjName = TH.mkName . show

sjPat :: SingJet -> TH.Pat
sjPat sj = TH.ConP (sjName sj) []

urPat :: Ur -> TH.Pat
urPat = \case
  S          -> TH.ConP 'S []
  K          -> TH.ConP 'K []
  J          -> TH.ConP 'J []
  D          -> TH.ConP 'D []
  SingJet sj -> TH.ConP 'SingJet [sjPat sj]
  DataJet dj -> TH.ConP 'DataJet [djPat dj]

valPat :: Val -> TH.Pat
valPat = \case
  N n    -> TH.ConP 'N [urPat n]
  x :& y -> TH.ConP '(:&) [valPat x, valPat y]

--------------------------------------------------------------------------------

jetArity :: TH.Exp
jetArity = TH.LamCaseE $ matches $ mapToList jetsMap
 where
  matches = fmap \(sj, (ari, _, _)) ->
    TH.Match (sjPat sj) (TH.NormalB $ intLit ari) []

jetTag :: TH.Exp
jetTag = TH.LamCaseE $ matches $ mapToList jetsMap
 where
  matches = fmap \(sj, (_, tag, _)) ->
    TH.Match (sjPat sj) (TH.NormalB $ valExp tag) []

jetBody :: TH.Exp
jetBody = TH.LamCaseE $ matches $ mapToList jetsMap
 where
  matches =
    fmap \(sj, (_, _, bod)) -> TH.Match (sjPat sj) (TH.NormalB $ valExp bod) []

jetMatch :: TH.Exp
jetMatch = TH.LamCaseE $ flip snoc fb $ mxs $ mapToList jetsMap
 where
  fb  = TH.Match TH.WildP (TH.NormalB (TH.ConE 'Nothing)) []
  mxs = fmap \(sj, (ari, tag, bod)) -> TH.Match
    (TH.TupP [TH.LitP (TH.IntegerL (fromIntegral ari)), valPat tag, valPat bod])
    (TH.NormalB (TH.AppE (TH.ConE 'Just) (TH.ConE (TH.mkName (show sj)))))
    []
