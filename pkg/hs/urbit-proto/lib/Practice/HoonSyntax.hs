module Practice.HoonSyntax where

import ClassyPrelude hiding (choice, many, try)

import Control.Monad.Combinators
import Control.Monad.Reader
import qualified Data.Set as Set
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Urbit.Atom (utf8Atom)

import Practice.HoonCommon (Atom, Aura, Term, Grit(..), Wing, Limb(..))

-- | Base type
data Bass
  = Non
  | Cel
  | Flg
  | Nul
  | Vod
  | Fok [Atom] Aura
  | Aur Aura
  deriving (Eq, Ord, Show)

data Hoon
  = Wung Wing
  | Adam Grit Atom Aura
  --
  | Bass Bass
  | Bcbc  -- ^ type of type
  | Bcbr (Map Term Spec)  -- ^ lead core type (prev $!)
  | Bccb Hoon  -- ^ type of expression
  | Bccl Spec [Spec]  -- ^ tuple
  | Bccn [(Atom, Aura, Spec)]  -- ^ tagged union
  | Bcdt Spec (Map Term Spec)  -- ^ gold core type, allegedly (may change this)
  | Bchp Spec Spec  -- ^ function
  | Bckt Spec Spec  -- ^ god save me
  | Bcts Skin Spec  -- ^ facialized type
  | Bcpt Spec Spec  -- ^ atomic-cellular disjunction
  --
  | Brcn (Map Term Hoon)
  | Brts Skin Hoon
  --
  | Clcb Hoon Hoon
  | Clkt Hoon Hoon Hoon Hoon
  | Clhp Hoon Hoon
  | Clls Hoon Hoon Hoon
  | Clsg [Hoon]
  | Cltr [Hoon]  -- XX actually should be run1
  --
  -- | Cncb  [%cncb p=wing q=(list (pair wing hoon))]            ::  %_
  | Cndt Hoon Hoon
  | Cnhp Hoon Hoon
  | Cncl Hoon [Hoon]
  -- | Cntr  [%cntr p=wing q=hoon r=(list (pair wing hoon))]     ::  %*
  | Cnkt Hoon Hoon Hoon Hoon
  | Cnls Hoon Hoon Hoon
  -- | Cnsg  [%cnsg p=wing q=hoon r=(list hoon)]                 ::  %~
  | Cnts Wing [(Wing, Hoon)]
  --
  | Dtkt Hoon Hoon
  | Dtls Hoon
  | Dttr Hoon Hoon
  | Dtts Hoon Hoon
  | Dtwt Hoon
  --
  | Ktls Hoon Hoon
  | Kthp Spec Hoon
  | Ktfs Hoon Spec
  -- | Ktsg fold constant?
  | Ktwt Hoon  -- ^ lead
  | Ktts Skin Hoon  -- ^ apply faces
  | Ktcl Spec  -- ^ mold
  | Ktzp Spec Hoon
  --
  | Sgfs Term Hoon
  --
  --
  | Tsfs Skin Hoon Hoon
  | Tsmc Skin Hoon Hoon  -- XX why is this not =\
  | Tsdt Wing Hoon Hoon
  | Tswt Wing Hoon Hoon Hoon
  | Tsgl Hoon Hoon
  | Tsgr Hoon Hoon
  | Tshp Hoon Hoon
  | Tskt Skin Wing Hoon Hoon
  | Tsls Hoon Hoon
  | Tssg [Hoon]
  -- | Tstr
  -- | Tscm
  --
  | Wtbr [Hoon]
  | Wthp Wing [(Skin, Hoon)]
  | Wtcl Hoon Hoon Hoon
  | Wtdt Hoon Hoon Hoon
  | Wtkt Wing Hoon Hoon
  | Wtgl Hoon Hoon
  | Wtgr Hoon Hoon
  -- | Wtls am not approve
  | Wtpm [Hoon]
  | Wtpt Wing Hoon Hoon
  | Wtts Skin Hoon
  | Wtzp Hoon
  --
  | Zpzp
  deriving (Eq, Ord, Show)

-- | Patterns
type Skin = Hoon

-- | Types
type Spec = Hoon

-- | That pattern which consists only of a variable
pattern Rash :: Term -> Skin
pattern Rash t = Wung [Ally t]


-- Parser ----------------------------------------------------------------------

data Mode = Wide | Tall
  deriving (Eq, Ord, Show)

type Parser = ReaderT Mode (Parsec Void Text)

parse :: Parser a -> FilePath -> Text -> Either Text a
parse p fp inp = bimap (pack . errorBundlePretty) id
               $ runParser (runReaderT p Tall) fp inp

-- | Require that the parser and all of its contents operate solely in wide
-- mode.
wide :: Parser a -> Parser a
wide = local (const Wide)

-- | Single space.
ace :: Parser ()
ace = char ' ' $> ()

-- | Plural space. XX why so complex?
gap :: Parser ()
gap = (char '\n' $> () <|> (gah *> (gah <|> vul) <|> vul))
   *> many (vul <|> gah)
   $> ()

gah :: Parser ()
gah = oneOf ['\n', ' '] $> ()

-- | Comments.
vul :: Parser ()
vul = string "::"
   *> many (satisfy \c -> c >= '\32' && c <= '\256' && c /= '\DEL')
   *> char '\n'
   $> ()

-- The actual parser -----------------------------------------------------------

hoon :: Parser Hoon
hoon = choice
  [ rune
  , scat
  ]

skin :: Parser Skin
skin = hoon

spec :: Parser Spec
spec = hoon

-- | Regular forms.
rune :: Parser Hoon
rune = choice $ fmap (\(r, b) -> string r *> b)
  [ ("$|", hop  Bcbr term spec)
  , ("$_", r1   Bccb hoon)
  , ("$:", run1 Bccl hoon hoon)
  , ("$%", run  id spec >>= bccn)
  , ("$.", hop1 Bcdt spec term spec)
  , ("$-", r2   Bchp spec spec)
  , ("$^", r2   Bckt spec spec)
  , ("$=", r2   Bcts skin spec)
  , ("$@", r2   Bcpt spec spec)
  --
  , ("|%", hop  Brcn term hoon)
  , ("|=", r2   Brts skin hoon)
  --
  , (":_", r2   Clcb hoon hoon)
  , (":^", r4   Clkt hoon hoon hoon hoon)
  , (":-", r2   Clhp hoon hoon)
  , (":+", r3   Clls hoon hoon hoon)
  , (":~", run  Clsg hoon)
  , (":*", run  Cltr hoon)
  --
  , ("%.", r2   Cndt hoon hoon)
  , ("%-", r2   Cnhp hoon hoon)
  , ("%:", run1 Cncl hoon hoon)
  , ("%^", r4   Cnkt hoon hoon hoon hoon)
  , ("%+", r3   Cnls hoon hoon hoon)
  , ("%=", jog1 Cnts wing wing hoon)
  --
  , (".^", r2   Dtkt hoon hoon)
  , (".+", r1   Dtls hoon)
  , (".*", r2   Dttr hoon hoon)
  , (".=", r2   Dtts hoon hoon)
  , (".?", r1   Dtwt hoon)
  --
  , ("^+", r2   Ktls hoon hoon)
  , ("^-", r2   Kthp spec hoon)
  , ("^/", r2   Ktfs hoon spec)
  , ("^?", r1   Ktwt hoon)
  , ("^=", r2   Ktts skin hoon)
  , ("^:", r1   Ktcl spec)
  , ("^!", r2   Ktzp spec hoon)
  --
  , ("~/", r2   Sgfs term hoon)
  --
  , ("=/", r3   Tsfs skin hoon hoon)
  , ("=;", r3   Tsmc skin hoon hoon)
  , ("=.", r3   Tsdt wing hoon hoon)
  , ("=<", r2   Tsgl hoon hoon)
  , ("=>", r2   Tsgr hoon hoon)
  , ("=-", r2   Tshp hoon hoon)
  , ("=^", r4   Tskt skin wing hoon hoon)
  , ("=+", r2   Tsls hoon hoon)
  , ("=~", run  Tssg hoon)
  --
  , ("?|", run  Wtbr hoon)
  , ("?-", jog1 Wthp wing skin hoon)
  , ("?:", r3   Wtcl hoon hoon hoon)
  , ("?.", r3   Wtdt hoon hoon hoon)
  , ("?^", r3   Wtkt wing hoon hoon)
  , ("?<", r2   Wtgl hoon hoon)
  , ("?>", r2   Wtgr hoon hoon)
  , ("?&", run  Wtpm hoon)
  , ("?@", r3   Wtpt wing hoon hoon)
  , ("?=", r2   Wtts skin hoon)
  , ("?!", r1   Wtzp hoon)
  ]
 where
  bccn :: [Hoon] -> Parser Hoon
  bccn ss = Bccn <$> concat <$> for ss \case
    Bccl _ [] -> fail "$% clause must be cellular"
    Bccl (Bass (Fok as au)) (t:ts) -> pure $ map (, au, Bccl t ts) as
    _ -> fail "$% clause must be a cell type with atomic head"

-- | Irregular forms.
scat :: Parser Hoon
scat = wide $ choice
  -- missing ,
  [ string "!!" $> Zpzp
  , char '!' *> hoon <&> Wtzp
  , char '_' *> hoon <&> Bccb  -- XX why ktcl bccb in orig?
  , char '$' *> rock (\a au -> Bass $ Fok [a] au)
  , char '%' *> rock (Adam Rock)  -- TODO posh porc
  , char '&' *> choice
    [ run Wtpm hoon
    , pure (Adam Sand 0 "f")
    ]
  , run1 Cncl hoon hoon
  , char '*' $> Bass Non
  , char '@' *> mote <&> Bass . Aur
  -- TODO lark syntax (soil?), rope
  , try $ char '+' *> r1 Dtls hoon
  -- XX '.'?
  -- TODO autonamer
  , char '=' *> r2 Dtts hoon hoon
  , char '?' *> choice
    [ run (\as -> Bass $ Fok as "") (char '$' *> rock \a _ -> a)  -- XX multiaura
    , pure (Bass $ Aur "f")
    ]
  , char '[' *> (Cltr <$> sepBy hoon ace) <* char ']'  -- XX read rupl
  , char '^' $> Bass Cel  -- XX why is there a wing case here?
  -- TODO ``

  , wing <&> Wung
  , sand
  ]



wing :: Parser Wing
wing = sepBy1 limb (char '.') <|> char '.' $> []

limb :: Parser Limb
limb = choice
  [ Ally <$> term
  , Axis <$> (char '+' *> L.decimal)
  ]

-- | Aura body.
mote :: Parser Term
mote = pack <$> ((<>) <$> many lowerChar <*> many upperChar)

term :: Parser Term
term = string "$" $> "" <|> do
  fist <- lowerChar
  rest <- many (char '-' <|> lowerChar <|> digitChar)
  pure $ pack (fist:rest)

sand :: Parser Hoon
sand = choice
  [ L.decimal <&> \d -> Adam Sand d "ud"  -- XX dots
  , cord      <&> \c -> Adam Sand (utf8Atom c) "t"
  ]

-- | Atomic constant body, for after % or $
rock :: (Atom -> Term -> a) -> Parser a
rock k = choice
    [ term       <&> \t -> k (utf8Atom t) "tas"
    , char '&'    $>       k 0 "f"
    , char '|'    $>       k 1 "f"
    , cord       <&> \t -> k (utf8Atom t) "t"
    , L.decimal  <&> \d -> k d "ud"
    -- , undefined -- XX nuck
    ]

-- XX actually follow syntax in ++qut
cord :: Parser Text
cord = pack <$> (char '\'' *> manyTill L.charLiteral (char '\''))

-- irregulars: ++scat and ++scad


-- Runic helpers ---------------------------------------------------------------

modally :: Parser a -> Parser a -> Parser a
modally tal wid = ask >>= \case
  Tall -> tal <|> wide wid
  Wide -> wid

r1 :: (a -> r) -> Parser a -> Parser r
r1 f p = modally
  do
    gap
    a <- p
    pure (f a)
  do
    char '('
    a <- p
    char ')'
    pure (f a)

r2 :: (a -> b -> r) -> Parser a -> Parser b -> Parser r
r2 f p q = modally
  do
    gap
    a <- p
    gap
    b <- q
    pure (f a b)
  do
    char '('
    a <- p
    ace
    b <- q
    char ')'
    pure (f a b)

r3 :: (a -> b -> c-> r) -> Parser a -> Parser b -> Parser c -> Parser r
r3 f p q r = modally
  do
    gap
    a <- p
    gap
    b <- q
    gap
    c <- r
    pure (f a b c)
  do
    char '('
    a <- p
    ace
    b <- q
    ace
    c <- r
    char ')'
    pure (f a b c)

r4 :: (a -> b -> c -> d -> r)
   -> Parser a -> Parser b -> Parser c -> Parser d -> Parser r
r4 f p q r s = modally
  do
    gap
    a <- p
    gap
    b <- q
    gap
    c <- r
    gap
    d <- s
    pure (f a b c d)
  do
    char '('
    a <- p
    ace
    b <- q
    ace
    c <- r
    ace
    d <- s
    char ')'
    pure (f a b c d)

run :: ([a] -> r) -> Parser a -> Parser r
run f p = modally
  (f <$> (gap *> manyTill (p <* gap) (string "==")))
  do
    char '('
    as <- sepBy p ace
    char ')'
    pure (f as)

run1 :: (a -> [b] -> r) -> Parser a -> Parser b -> Parser r
run1 f p q = modally
  (f <$> (gap *> p <* gap) <*> manyTill (q <* gap) (string "==") <* gap)
   do
    char '('
    a <- p
    bs <- many (ace *> q)
    pure (f a bs)

jog :: ([(a, b)] -> r) -> Parser a -> Parser b -> Parser r
jog f p q = ask >>= \case
  Tall -> do
    gap
    abs <- flip manyTill (string "==") do
      a <- p
      gap
      b <- q
      gap
      pure (a, b)
    pure (f abs)
  Wide -> empty

jog1 :: (a -> [(b, c)] -> r) -> Parser a -> Parser b -> Parser c -> Parser r
jog1 f p q r = ask >>= \case
  Tall -> do
    gap
    a <- p
    gap
    bcs <- flip manyTill (string "==") do
      b <- q
      gap
      c <- r
      gap
      pure (b, c)
    pure (f a bcs)
  Wide -> empty

-- | Parse the syntax of a core body.
hop :: Ord a => (Map a b -> r) -> Parser a -> Parser b -> Parser r
hop f p q = ask >>= \case
  Tall -> do
    gap
    abs <- flip manyTill (string "--") do
      string "++"
      gap
      a <- p
      gap
      b <- q
      gap
      pure (a, b)
    when (length (Set.fromList (map fst abs)) /= length abs) $
      fail "duplicate arm name in hopping body"
    pure (f $ mapFromList abs)
  Wide -> empty

hop1 :: Ord b
     => (a -> Map b c -> r)
     -> Parser a -> Parser b -> Parser c-> Parser r
hop1 f p q r = ask >>= \case
  Tall -> do
    gap
    a <- p
    gap
    bcs <- flip manyTill (string "--") do
      string "++"
      gap
      b <- q
      gap
      c <- r
      gap
      pure (b, c)
    when (length (Set.fromList (map fst bcs)) /= length bcs) $
      fail "duplicate arm name in hopping body"
    pure (f a $ mapFromList bcs)
  Wide -> empty
