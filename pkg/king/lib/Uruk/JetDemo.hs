{-
    This is an extremely simple (and very, very slow) Uruk evaluator.

    It evaluates Uruk by applying reduction rules until we reach a normal
    form, and printing the expression after every reduction.

    This is an extremely dumb evaluation strategy, but it's dead simple
    and closely matches the spec. It's also a useful debugging tool,
    since it shows each reduction step.
-}

module Uruk.JetDemo
    ( Ur(..)
    , normalize
    , reduce
    , jam
    , church
    , unChurch
    , dash
    , j1, j2, j3, j4
    , j_nat
    , j_zer, zer
    , j_inc, inc
    , j_one, one
    , j_fol, fol
    , j_dec, dec
    , j_add, add
    , j_lef, lef
    , j_rit, rit
    , j_cas, cas
    , Match(..)
    , valUr
    , jetExp
    ) where

import ClassyPrelude

-- ort Control.Arrow ((>>>))
import Data.Bits
import Data.Function    ((&))
import GHC.Natural      (Natural)


-- Types -----------------------------------------------------------------------

infixl 5 :@;

data Ur
    = S
    | K
    | D
    | J Natural
    | Nat Natural
    | Fol
    | Add
    | Inc
    | Dec
    | Lef
    | Rit
    | Cas
    | Ur :@ Ur
    | Jc Natural Ur Ur [Ur]
  deriving (Eq, Ord)

jetExpand ∷ Natural → Ur
jetExpand = go
  where go = \case { 0 → no; 1 → J 1; n → go (pred n) :@ J 1 }
        no = error "impossible J value"

jetUnclosure ∷ Natural → Ur → Ur → [Ur] → Ur
jetUnclosure n tag bod = go (J n :@ tag :@ bod) . reverse
  where go acc = \case { [] → acc; x:xs → go (acc :@ x) xs }

instance Show Ur where
    show = \case
        K           → "1"
        S           → "2"
        D           → "3"
        Nat n       → "#" <> show n
        Add         → "add"
        Inc         → "inc"
        Dec         → "dec"
        Lef         → "lef"
        Rit         → "rit"
        Fol         → "fol"
        Cas         → "case"
        J n         → replicate (fromIntegral n) '0'
        Jc n t b xs → close n t b xs
        x :@ y → "[" <> intercalate " " (show <$> flatten x [y]) <> "]"
      where
        flatten (x :@ y) acc = flatten x (y : acc)
        flatten x        acc = (x : acc)

        close n t b = \case
            [] → mconcat [ "{", show (J n), " ", show t, " ", show b, "}" ]
            xs → mconcat [ "<"
                         , close n t b []
                         , " "
                         , intercalate " " (show <$> reverse xs)
                         , ">"
                         ]


-- Normalized Values -----------------------------------------------------------

newtype Val = MkVal { valUr ∷ Ur }
  deriving newtype (Eq, Ord, Show)

urVal ∷ Ur → Val
urVal ur =
    reduce ur & \case
        Nothing → MkVal ur
        Just ru → urVal ru


-- Named Functions -------------------------------------------------------------

data Named a = Named { _nName ∷ String, unNamed ∷ a }

instance Show (Named a) where
  show (Named s _) = s



--------------------------------------------------------------------------------

data Match = MkMatch
    { mFast ∷ Val
    , mArgs ∷ Word
    , mName ∷ Val
    , mBody ∷ Val
    }

match ∷ Ur → Natural → Ur → Ur → Match
match j n t b = MkMatch (urVal j) (fromIntegral n) (urVal t) (urVal b)

data Check = MkCheck
    { cArgs ∷ Word
    , cName ∷ Val
    , cPred ∷ Named (Val → Maybe Val)
    }

check ∷ String → Natural → Ur → (Val → Maybe Ur) → Check
check nm n t p = MkCheck (fromIntegral n) (urVal t)
               $ Named nm (fmap urVal <$> p)

type DashEntry = ((Word, Val), Named (Val → Maybe Val))
type DashBoard = Map (Word, Val) [Named (Val → Maybe Val)]

simpleEnt ∷ Match → DashEntry
simpleEnt MkMatch{..} =
    (,) (mArgs, mName)
        (Named (show mBody) (\b → guard (b==mBody) $> mFast))

predikEnt ∷ Check → DashEntry
predikEnt MkCheck{..} = ((cArgs, cName), cPred)

mkDash ∷ [DashEntry] → DashBoard
mkDash = foldl' go mempty
  where
    go ∷ DashBoard → DashEntry → DashBoard
    go acc (k,v) = lookup k acc & \case
                       Nothing → insertMap k [v] acc
                       Just vs → insertMap k (v:vs) acc

dashLookup ∷ Natural → Ur → Ur → Maybe Ur
dashLookup n t b = lookup (fromIntegral n, MkVal t) dash & \case
    Nothing → Nothing
    Just xs → valUr <$> asum (($ b') . unNamed <$> xs)
  where
    b' = urVal b


-- Jet Dashboard ---------------------------------------------------------------

unChurch ∷ Ur → Maybe Natural
unChurch = \case
    S :@ K                   -> Just 0
    S :@ (S:@(K:@S):@K) :@ n -> succ <$> unChurch n
    _                        -> Nothing

emp ∷ Ur
emp = K

pattern I = S :@ K :@ K

j1 = J 1 :@ K
j2 = J 2 :@ K
j3 = J 3 :@ K
j4 = J 4 :@ K

--  zer i z = z
--  suc n = \i z → i (n i z)
--  one = inc zer
--  fol n = n inc zer
--  inc n = jet1 (\i z → i (n i z))
j_zer = match (Nat 0) 2 emp (S :@ K)
j_one = match (Nat 1) 2 emp (S :@ (S:@(K:@S):@K) :@ (S:@K))
j_fol = match Fol     1 emp (S:@(S:@I:@(K:@(S:@(S:@(K:@S):@K)))):@(K:@(S:@K)))
j_inc = match Inc     1 emp (S:@(K:@j2):@(S:@(K:@(S:@(S:@(K:@S):@K))):@Fol))
j_dec = match Dec     1 emp (D :@ D :@ D)
j_add = match Add     2 emp (S:@(S:@I:@(K:@Inc)):@(S:@(S:@I:@(K:@Inc)):@(K:@(S:@K))))
j_nat = check "nat" 2 K (fmap Nat <$> unChurch . valUr)

-- fol = (S:@(S:@(S:@K:@K):@(K:@(S:@(S:@(K:@S):@K)))):@(K:@(S:@K)))

--  left x l r = l x
--  right x l r = r x
--  case b l r = b l r
j_lef = match Lef 3 emp (S:@(K:@(S:@(K:@(S:@(K:@K))):@(S:@I))):@K)
j_rit = match Rit 3 emp (S:@(K:@(S:@(K:@K):@(S:@I))):@K)
j_cas = match Cas 3 emp I

zer = jetExp j_zer
one = jetExp j_one
fol = jetExp j_fol
inc = jetExp j_inc
lef = jetExp j_lef
dec = jetExp j_dec
add = jetExp j_add
rit = jetExp j_rit
cas = jetExp j_cas

dash ∷ DashBoard
dash = mkDash
    [ simpleEnt j_zer
    , simpleEnt j_one
    , simpleEnt j_fol
    , simpleEnt j_inc
    , simpleEnt j_add
    , simpleEnt j_lef
    , simpleEnt j_rit
    , predikEnt j_nat
    ]



-- Evaluation ------------------------------------------------------------------

--
--  Repeatedly perform reductions until the input is fully normalized.
--
normalize ∷ Ur → IO Ur
normalize ur = do
    putStrLn (">>  " <> tshow ur)
    reduce ur & \case
        Nothing -> pure ur
        Just ru -> normalize ru

--
--  Perform one reduction step. Return Nothing if the input is fully
--  normalized.
--
reduce ∷ Ur → Maybe Ur
reduce = \case
    (reduce → Just xv) :@ y → Just (xv :@ y)
    x :@ (reduce → Just yv) → Just (x  :@ yv)

    -- Uruk
    K :@ x :@ y      → Just x
    S :@ x :@ y :@ z → Just (x:@z:@(y:@z))
    D :@ x           → Just (jam x)
    J n :@ J 1       → Just (J (succ n))
    J n :@ t :@ b    → dashLookup n t b <|> pure (Jc n t b [])

    Jc n t b xs | n==len xs → Just (apply b xs)
    Jc n t b xs :@ x        → Just (Jc n t b (x:xs))

    --  Fire jet
    Nat n :@ x :@ y → Just (church n :@ x :@ y)

    Inc :@ Nat n → Just (Nat (succ n))
    Inc :@ x     → Just (jetBod j_inc :@ x)

    Add :@ Nat x :@ Nat y → Just (Nat (x+y))
    Add :@ _     :@ _     → error "bad-add"

    --  Doesn't reduce
    _ → Nothing
  where
    len = fromIntegral . length

    apply ∷ Ur → [Ur] → Ur
    apply f = go f . reverse
      where go acc = \case { [] → acc; x:xs → go (acc :@ x) xs }

jetBod ∷ Match → Ur
jetBod = valUr . mBody

jetExp ∷ Match → Ur
jetExp (MkMatch _ n t b) = J (fromIntegral n) :@ valUr t :@ valUr b

--
--  Produces a jetted, church-encoded natural number.
--
church ∷ Natural → Ur
church 0 = S :@ K
church n = S :@ (S:@(K:@S):@K) :@ church (pred n)

churchJet ∷ Natural → Ur
churchJet n = J 2 :@ K :@ church n

--
--  Serialize and Uruk expression and church-encode it.
--
jam ∷ Ur → Ur
jam = churchJet . snd . go
  where
    go ∷ Ur → (Int, Natural)
    go Inc           = go (jetExp j_inc)
    go Fol           = go (jetExp j_fol)
    go Dec           = go (jetExp j_dec)
    go Add           = go (jetExp j_add)
    go Lef           = go (jetExp j_lef)
    go Rit           = go (jetExp j_rit)
    go Cas           = go (jetExp j_cas)
    go (Nat n)       = go (churchJet n)
    go S             = (3, 0)
    go K             = (3, 2)
    go D             = (3, 4)
    go (J 1)         = (3, 6)
    go (J n)         = go (jetExpand n)
    go (Jc n t b xs) = go (jetUnclosure n t b xs)
    go (x:@y)  = (rBits, rNum)
        where (xBits, xNum) = go x
              (yBits, yNum) = go y
              rBits = 1 + xBits + yBits
              rNum  = 1 .|. shiftL xNum 1
                        .|. shiftL yNum (1+xBits)
