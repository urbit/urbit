{-
    This is an extremely simple (and very, very slow) Uruk evaluator.

    It evaluates Uruk by applying reduction rules until we reach a normal
    form, and printing the expression after every reduction.

    This is an extremely dumb evaluation strategy, but it's dead simple
    and closely matches the spec. It's also a useful debugging tool,
    since it shows each reduction step.
-}

module Uruk.Demo
    ( Ur(..)
    , normalize
    , reduce
    , jam
    , church
    , jet
    ) where

import ClassyPrelude
import Data.Bits

import Data.Function ((&))
import GHC.Natural   (Natural)
import Noun.Core     (textToUtf8Atom, pattern Atom)


-- Types -----------------------------------------------------------------------

infixl 5 :@;

data Ur
    = J
    | S
    | K
    | D
    | Ur :@ Ur
    | Jn Natural
    | Jc Natural Ur Ur [Ur]
  deriving (Eq, Ord)

jetExpand ∷ Natural → Ur
jetExpand = go
  where go = \case { 0 → no; 1 → J; n → go (pred n) :@ J }
        no = error "impossible Jn value"

jetUnclosure ∷ Natural → Ur → Ur → [Ur] → Ur
jetUnclosure n tag bod = go (Jn n :@ tag :@ bod) . reverse
  where go acc = \case { [] → acc; x:xs → go (acc :@ x) xs }

instance Show Ur where
    show = \case
        J           → "0"
        K           → "1"
        S           → "2"
        D           → "3"
        Jn n        → replicate (fromIntegral n) '0'
        Jc n t b xs → close n t b xs
        x :@ y → "[" <> intercalate " " (show <$> flatten x [y]) <> "]"
      where
        flatten (x :@ y) acc = flatten x (y : acc)
        flatten x        acc = (x : acc)

        close n t b = \case
            [] → mconcat [ "{", show (Jn n), " ", show t, " ", show b, "}" ]
            xs → mconcat [ "["
                         , close n t b []
                         , " "
                         , intercalate " " (show <$> reverse xs)
                         , "]"
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
    (reduce -> Just xv) :@ y  → Just (xv :@ y)
    x :@ (reduce -> Just yv)  → Just (x :@ yv)
    K:@x:@y                   → Just x
    S:@x:@y:@z                → Just (x:@z:@(y:@z))
    D:@x                      → Just (jam x)
    J:@J                      → Just (Jn 2)
    Jn n:@J                   → Just (Jn (succ n))
    J    :@ t :@ b            → Just (Jc 1 t b [])
    Jn n :@ t :@ b            → Just (Jc n t b [])
    Jc n t b xs | n == len xs → Just (jetApp b xs)
    Jc n t b xs :@ x          → Just (Jc n t b (x:xs))
    _                         → Nothing
  where
    len = fromIntegral . length
    jetApp f = go f . reverse
      where go acc = \case { [] → acc; x:xs → go (acc :@ x) xs }

--
--  Produces a jetted, church-encoded natural number.
--
church ∷ Natural → Ur
church = jetNat . go
  where
    go 0 = S :@ K
    go n = S :@ (S:@(K:@S):@K) :@ go (pred n)

    jetNat = (Jn 1 :@ K :@)

jet 0 _  b = b
jet n nm b = Jn n :@ cord nm :@ b

cord ∷ Text → Ur
cord = church . x . textToUtf8Atom
  where x (Atom a) = a
        x _        = error "this will never happen"

--
--  Serialize and Uruk expression and church-encode it.
--
jam ∷ Ur → Ur
jam = church . snd . go
  where
    go ∷ Ur → (Int, Natural)
    go J             = (3, 0)
    go K             = (3, 2)
    go S             = (3, 4)
    go D             = (3, 6)
    go (Jn n)        = go (jetExpand n)
    go (Jc n t b xs) = go (jetUnclosure n t b xs)
    go (x:@y)        = (rBits, rNum)
        where (xBits, xNum) = go x
              (yBits, yNum) = go y
              rBits = 1 + xBits + yBits
              rNum  = 1 .|. shiftL xNum 1
                        .|. shiftL yNum (1+xBits)
