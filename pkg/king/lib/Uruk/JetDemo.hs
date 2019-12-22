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
    , pattern ZerJet
    , pattern OneJet
    , pattern FolJet
    , pattern IncJet
    , pattern DecJet
    , pattern AddJet
    , pattern LefJet
    , pattern RitJet
    , pattern CasJet
    , pattern Jet1
    , pattern Jet2
    , pattern Jet3
    , pattern Jet4
    , dash
    ) where

import ClassyPrelude

import Data.Bits
import Data.Function    ((&))
import GHC.Natural      (Natural)
import System.IO.Unsafe (unsafePerformIO)


-- Types -----------------------------------------------------------------------

infixl 5 :@;

data Ur
    = S
    | K
    | D
    | J
    | Nat Natural
    | Add
    | Inc
    | Dec
    | Lef
    | Rit
    | Cas
    | Ur :@ Ur
    | Jn Natural
    | Jc Natural Ur Ur [Ur]
  deriving (Eq, Ord, Show)

jetExpand ∷ Natural → Ur
jetExpand = go
  where go = \case { 0 → no; 1 → J; n → go (pred n) :@ J }
        no = error "impossible Jn value"

jetUnclosure ∷ Natural → Ur → Ur → [Ur] → Ur
jetUnclosure n tag bod = go (Jn n :@ tag :@ bod) . reverse
  where go acc = \case { [] → acc; x:xs → go (acc :@ x) xs }

{-
instance Show Ur where
    show = \case
        J           → "0"
        K           → "1"
        S           → "2"
        D           → "3"
        Nat n       → "#" <> show n
        Add         → "add"
        Inc         → "inc"
        Dec         → "dec"
        Lef         → "lef"
        Rit         → "rit"
        Cas         → "case"
        Jn n        → replicate (fromIntegral n) '0'
        Jc n t b xs → close n t b xs
        x :@ y → "[" <> intercalate " " (show <$> flatten x [y]) <> "]"
      where
        flatten (x :@ y) acc = flatten x (y : acc)
        flatten x        acc = (x : acc)

        close n t b = \case
            [] → mconcat [ "{", show (Jn n), " ", show t, " ", show b, "}" ]
            xs → mconcat [ "<"
                         , close n t b []
                         , " "
                         , intercalate " " (show <$> reverse xs)
                         , ">"
                         ]
-}

-- Jets ------------------------------------------------------------------------

pattern I = S :@ K :@ K

pattern Jet1 = Jn 1 :@ K
pattern Jet2 = Jn 2 :@ K
pattern Jet3 = Jn 3 :@ K
pattern Jet4 = J :@ J :@ J :@ J :@ K

--  zer i z = z
--  suc n = \i z → i (n i z)
--  one = inc zer
--  fol n = n inc zer
--  inc n = jet1 (\i z → i (n i z))
pattern ZerJet = Jc 2 K (S :@ K) []
pattern OneJet = Jc 2 K (S:@(S:@(K:@S):@K):@(S:@K)) []
pattern FolJet = Jc 1 K (S:@(S:@(S:@K:@K):@(K:@(S:@(S:@(K:@S):@K)))):@(K:@(S:@K))) []

pattern IncJet = Jc 1 K (S:@(K:@Jet2):@(S:@(K:@(S:@(S:@(K:@S):@K))):@FolJet)) []


pattern DecJet = D
pattern AddJet = Jc 2 K (S:@(S:@I:@(K:@Inc)):@(S:@(S:@I:@(K:@Inc)):@(K:@(S:@K)))) []

--  left x l r = l x
--  right x l r = r x
--  case b l r = b l r
pattern LefJet = Jc 3 K (S:@(K:@(S:@(K:@(S:@(K:@K))):@(S:@I))):@K) []
pattern RitJet = Jc 3 K (S:@(K:@(S:@(K:@K):@(S:@I))):@K) []
pattern CasJet = Jc 3 K I []

natJet ∷ Ur → Maybe Natural
natJet = \case
    Jc 2 K (unChurch -> Just n) [] -> Just n
    _                              -> Nothing
  where
    unChurch = \case
        S :@ K                   -> Just 0
        S :@ (S:@(K:@S):@K) :@ n -> succ <$> unChurch n
        _                        -> Nothing


-- Evaluation ------------------------------------------------------------------

--
--  Repeatedly perform reductions until the input is fully normalized.
--
normalize ∷ Bool → Ur → IO Ur
normalize doJet ur = do
    putStrLn (">>  " <> tshow ur)
    reduce doJet ur & \case
        Nothing -> pure ur
        Just ru -> normalize doJet ru

--
--  Perform one reduction step. Return Nothing if the input is fully
--  normalized.
--
reduce ∷ Bool → Ur → Maybe Ur
reduce doJet = \case
    (reduce doJet → Just xv) :@ y → Just (xv :@ y)
    x :@ (reduce doJet → Just yv) → Just (x  :@ yv)

    -- Uruk
    K :@ x :@ y      → Just x
    S :@ x :@ y :@ z → Just (x:@z:@(y:@z))
    D :@ x           → Just (jam x)
    J :@ J           → Just (Jn 2)
    Jn n :@ J        → Just (Jn (succ n))
    J    :@ t :@ b   → Just (Jc 1 t b [])
    Jn n :@ t :@ b   → Just (Jc n t b [])

    v@(Jc n t b xs) → case (jet v, n == len xs) of
                        ( Just j,  _    )  → Just j
                        ( Nothing, True  ) → Just (apply b xs)
                        ( Nothing, False ) → Nothing

    Jc n t b xs :@ x → Just (Jc n t b (x:xs))

    --  Fire jet
    Inc :@ Nat n → Just (Nat (succ n))
    Inc :@ _     → error "bad-inc"

    Add :@ Nat x :@ Nat y → Just (Nat (x+y))
    Add :@ _     :@ _     → error "bad-add"

    --  Doesn't reduce
    _ → Nothing
  where
    len = fromIntegral . length

    apply ∷ Ur → [Ur] → Ur
    apply f = go f . reverse
      where go acc = \case { [] → acc; x:xs → go (acc :@ x) xs }

    jet = \case
      Jc n t b xs → do let v = Jc n t b []
                       j <- lookup v dash <|> Nat <$> natJet v
                       traceM (show (apply j xs))
                       Nothing -- (apply j xs)
      _           → Nothing

dash ∷ Map Ur Ur
dash = mapFromList
    [ (simpJet IncJet, Inc)
    , (simpJet AddJet, Add)
--  , (simpJet DecJet, Dec)
--  , (simpJet LefJet, Lef)
--  , (simpJet RitJet, Rit)
--  , (simpJet CasJet, Cas)
    ]
  where
    simpJet ∷ Ur → Ur
    simpJet (Jc n t b xs) = Jc n t' b' xs
      where t' = unsafePerformIO (normalize True t)
            b' = unsafePerformIO (normalize True b)
    simpJet x = error (show x)

--
--  Produces a jetted, church-encoded natural number.
--
church ∷ Natural → Ur
church = jetNat . go
  where
    go 0 = S :@ K
    go n = S :@ (S:@(K:@S):@K) :@ go (pred n)

    jetNat x = Jc 1 K x []

--
--  Serialize and Uruk expression and church-encode it.
--
jam ∷ Ur → Ur
jam = church . snd . go
  where
    go ∷ Ur → (Int, Natural)
    go Inc           = go IncJet
    go Dec           = go DecJet
    go Add           = go AddJet
    go Lef           = go LefJet
    go Rit           = go RitJet
    go Cas           = go CasJet
    go (Nat n)       = go (church n)
    go S             = (3, 0)
    go K             = (3, 2)
    go D             = (3, 4)
    go J             = (3, 6)
    go (Jn n)        = go (jetExpand n)
    go (Jc n t b xs) = go (jetUnclosure n t b xs)
    go (x:@y)  = (rBits, rNum)
        where (xBits, xNum) = go x
              (yBits, yNum) = go y
              rBits = 1 + xBits + yBits
              rNum  = 1 .|. shiftL xNum 1
                        .|. shiftL yNum (1+xBits)
