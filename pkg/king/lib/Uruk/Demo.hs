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
    | Ur :@ Ur
  deriving (Eq, Ord)

instance Show Ur where
    show = \case
        S      → "0"
        K      → "1"
        D      → "2"
        J      → "3"
        x :@ y → "[" <> intercalate " " (show <$> flatten x [y]) <> "]"
      where
        flatten (x :@ y) acc = flatten x (y : acc)
        flatten x        acc = (x : acc)


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
    S:@x:@y:@z             -> Just (x:@z:@(y:@z))
    K:@x:@y                -> Just x
    D:@x                   -> reduce x & \case
                                Nothing -> Just (jam x)
                                Just xv -> Just (D :@ xv)
    J:@S:@n:@b:@p          -> Just (b:@p)
    J:@K:@n:@b:@p:@q       -> Just (b:@p:@q)
    J:@D:@n:@b:@p:@q:@r    -> Just (b:@p:@q:@r)
    J:@J:@n:@b:@p:@q:@r:@s -> Just (b:@p:@q:@r:@s)
    x:@y                   -> case (reduce x, reduce y) of
                                (Nothing, Nothing) -> Nothing
                                (Just xv, _      ) -> Just (xv :@ y)
                                (Nothing, Just yv) -> Just (x :@ yv)
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
    go S      = (3, 0)
    go K      = (3, 2)
    go D      = (3, 4)
    go J      = (3, 6)
    go (x:@y) = (rBits, rNum)
        where (xBits, xNum) = go x
              (yBits, yNum) = go y
              rBits = 1 + xBits + yBits
              rNum  = 1 .|. shiftL xNum 1
                        .|. shiftL yNum (1+xBits)
