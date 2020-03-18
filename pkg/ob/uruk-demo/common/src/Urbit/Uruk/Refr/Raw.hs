module Urbit.Uruk.Refr.Raw (Exp(..), Ur(..), reduce, eval) where

import Prelude ()
import ClassyPrelude
import GHC.Natural
import Data.Bits
import Data.Tree

infixl 5 :@;
data Ur = J | K | S | D deriving (Eq, Ord, Enum, Show)
data Exp = N Ur | Exp :@ Exp deriving (Eq, Ord)

tree ∷ Exp → Tree Ur
tree = go [] where go a = \case { N n → Node n a; x :@ y → go (tree y:a) x }

unTree ∷ Tree Ur → Exp
unTree (Node n xs) = foldl' (:@) (N n) (unTree <$> xs)

showTree ∷ Tree Ur -> String
showTree (Node n []) = show n
showTree (Node n xs) = "(" <> intercalate " " (show n : fmap showTree xs) <> ")"

instance Show Exp where show = showTree . tree

eval ∷ Exp → Exp
eval = \case { (reduce→Just x) → eval x; x → x }

reduce ∷ Exp → Maybe Exp
reduce = \case
  N K :@ x :@ y         → Just x
  (reduce→Just xv) :@ y → Just (xv :@ y)
  x :@ (reduce→Just yv) → Just (x :@ yv)
  N S :@ x :@ y :@ z    → Just (x :@ z :@ (y :@ z))
  N D :@ x              → Just (jam x)
  (jetRule→Just(b,xs))  → Just (foldl' (:@) b xs)
  _                     → Nothing

jetRule ∷ Exp → Maybe (Exp, [Exp])
jetRule x = do
  (n, rest) ← jetHead (tree x)
  (b, xs)   ← case rest of { t:b:xs → Just (b,xs); _ → Nothing }
  guard (fromIntegral n == length xs)
  Just (unTree b, unTree <$> xs)

jetHead ∷ Tree Ur → Maybe (Natural, [Tree Ur])
jetHead (Node n xs) = guard (n == J) $> go 1 xs
 where go n (Node J [] : xs) = go (succ n) xs
       go n xs             = (n, xs)

jam ∷ Exp → Exp
jam = (N J :@ N J :@ N K :@) . enc . snd . go
 where
  enc 0 = N S :@ N K
  enc n = N S :@ (N S :@ (N K :@ N S) :@ N K) :@ enc (pred n)

  go (N n)    = (3, fromIntegral (fromEnum n*2))
  go (x :@ y) = (rBits ∷ Int, rNum ∷ Natural)
   where
    ((xBits, xNum), (yBits, yNum)) = (go x, go y)
    rBits = 1 + xBits + yBits
    rNum  = 1 .|. shiftL xNum 1 .|. shiftL yNum (1 + xBits)
