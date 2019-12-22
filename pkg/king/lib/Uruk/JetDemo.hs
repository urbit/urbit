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
    ) where

import ClassyPrelude

import Data.Function ((&))
import GHC.Natural (Natural)
import Data.Bits


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
  deriving (Eq, Ord)

instance Show Ur where
    show = \case
        S      → "0"
        K      → "1"
        D      → "2"
        J      → "3"
        Nat n  → "#" <> show n
        Add    → "add"
        Inc    → "inc"
        Dec    → "dec"
        Lef    → "lef"
        Rit    → "rit"
        Cas    → "case"
        x :@ y → "[" <> intercalate " " (show <$> flatten x [y]) <> "]"
      where
        flatten (x :@ y) acc = flatten x (y : acc)
        flatten x        acc = (x : acc)


-- Jets ------------------------------------------------------------------------

pattern I = S :@ K :@ K

pattern Jet1 = J :@ S :@ S
pattern Jet2 = J :@ K :@ S
pattern Jet3 = J :@ D :@ S
pattern Jet4 = J :@ J :@ S

pattern ZerJet = Jet2 :@ (S :@ K)
pattern IncJet = Jet1 :@ (S:@(K:@(S:@(K:@(S:@(K:@Jet2))))):@(S:@(S:@(K:@S):@K)))

pattern DecJet = (D :@ D :@ D)
pattern AddJet = S:@(S:@I:@(K:@Inc)):@(S:@(S:@I:@(K:@Inc)):@(K:@(S:@K)))

--  left x l r = l x
--  right x l r = r x
--  case b l r = b l r
pattern LefJet = Jet3 :@ (S:@(K:@(S:@(K:@(S:@(K:@K))):@(S:@I))):@K)
pattern RitJet = Jet3 :@ (S:@(K:@(S:@(K:@K):@(S:@I))):@K)
pattern CasJet = Jet3 :@ I

natJet ∷ Ur → Maybe Natural
natJet = \case
    J :@ K :@ S :@ (unChurch -> Just n) -> Just n
    _                                   -> Nothing
  where
    unChurch = \case
        S :@ K                   -> Just 0
        S :@ (S:@(K:@S):@K) :@ n -> succ <$> unChurch n
        _                        -> Nothing


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
    (reduce → Just xv) :@ y -> Just (xv :@ y)
    x :@ (reduce → Just yv) -> Just (x  :@ yv)

    Inc :@ Nat n            -> Just (Nat (succ n))
    Add :@ Nat x :@ Nat y   -> Just (Nat (x+y))

    S:@x:@y:@z              -> Just (x:@z:@(y:@z))
    K:@x:@y                 -> Just x
    D:@x                    -> reduce x & \case Nothing -> Just (jam x)
                                                Just xv -> Just (D :@ xv)

    IncJet                  -> Just Inc
    AddJet                  -> Just Add
    DecJet                  -> Just Dec
    LefJet                  -> Just Lef
    RitJet                  -> Just Rit
    CasJet                  -> Just Cas
    (natJet -> Just n)      -> Just (Nat n)

    J:@S:@n:@b:@p          -> Just (b:@p)
    J:@K:@n:@b:@p:@q       -> Just (b:@p:@q)
    J:@D:@n:@b:@p:@q:@r    -> Just (b:@p:@q:@r)
    J:@J:@n:@b:@p:@q:@r:@s -> Just (b:@p:@q:@r:@s)
    _                      -> Nothing

--
--  Produces a jetted, church-encoded natural number.
--
church ∷ Natural → Ur
church = jetNat . go
  where
    go 0 = S :@ K
    go n = S :@ (S:@(K:@S):@K) :@ go (pred n)

    jetNat n = J :@ K :@ S :@ n

--
--  Serialize and Uruk expression and church-encode it.
--
jam ∷ Ur → Ur
jam = church . snd . go
  where
    go ∷ Ur → (Int, Natural)
    go Inc     = go IncJet
    go Dec     = go DecJet
    go Add     = go AddJet
    go Lef     = go LefJet
    go Rit     = go RitJet
    go Cas     = go CasJet
    go (Nat n) = go (church n)
    go S       = (3, 0)
    go K       = (3, 2)
    go D       = (3, 4)
    go J       = (3, 6)
    go (x:@y)  = (rBits, rNum)
        where (xBits, xNum) = go x
              (yBits, yNum) = go y
              rBits = 1 + xBits + yBits
              rNum  = 1 .|. shiftL xNum 1
                        .|. shiftL yNum (1+xBits)
