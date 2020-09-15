module Urbit.Uruk.Reference (Exp(..), Ur(..), reduce, eval) where

import ClassyPrelude
import Data.Bits
import Data.Tree
import GHC.Natural

infixl 5 :@;
data Ur = S | K | E | W deriving (Eq, Ord, Enum, Show)
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
  N W :@ a :@ s :@ k :@ e :@ w :@ (x :@ y) -> Just (a :@ x :@ y)
  N W :@ a :@ s :@ k :@ e :@ w :@ N S      -> Just s
  N W :@ a :@ s :@ k :@ e :@ w :@ N K      -> Just k
  N W :@ a :@ s :@ k :@ e :@ w :@ N E      -> Just e
  N W :@ a :@ s :@ k :@ e :@ w :@ N W      -> Just w
  (jetRule→Just(b,xs))  → Just (foldl' (:@) b xs)
  _                     → Nothing

jetRule ∷ Exp → Maybe (Exp, [Exp])
jetRule x = do
  (n, rest) ← jetHead (tree x)
  (b, xs)   ← case rest of { t:b:xs → Just (b,xs); _ → Nothing }
  guard (fromIntegral n == length xs)
  Just (unTree b, unTree <$> xs)

jetHead ∷ Tree Ur → Maybe (Natural, [Tree Ur])
jetHead (Node n xs) = guard (n == E) $> go 1 xs
 where go n (Node E [] : xs) = go (succ n) xs
       go n xs               = (n, xs)
