module Urbit.Uruk.Refr.Demo (Exp(..), Ur(..), reduce, eval, exec) where

import ClassyPrelude
import GHC.Natural
import Data.Bits
import Data.Tree

infixl 5 :@;
data Ur = J | K | S | D | V Text deriving (Eq, Ord)
data Exp = N Ur | Exp :@ Exp deriving (Eq, Ord)

instance Show Ur where
  show = \case { J→"J"; K→"K"; S→"S"; D→"D"; V v→unpack v }

tree ∷ Exp → Tree Ur
tree = go [] where go a = \case { N n → Node n a; x :@ y → go (tree y:a) x }

unTree ∷ Tree Ur → Exp
unTree (Node n xs) = foldl' (:@) (N n) (unTree <$> xs)

showTree ∷ Tree Ur -> String
showTree (Node n []) = show n
showTree (Node n xs) = "(" <> intercalate " " (show n : fmap showTree xs) <> ")"

instance Show Exp where show = showTree . tree

eval ∷ (Text → Maybe Exp) → Exp → Exp
eval env = \case { (reduce env → Just x) → eval env x; x → x }

exec ∷ (Text → Maybe Exp) → Exp → [Exp]
exec env x = x : fromMaybe [] (fmap (exec env) (reduce env x))

reduce ∷ (Text → Maybe Exp) → Exp → Maybe Exp
reduce env = go
 where
  go = \case
    N (V x)               → env x
    N K :@ x :@ y         → Just x
    (go→Just xv) :@ y     → Just (xv :@ y)
    x :@ (go→Just yv)     → Just (x :@ yv)
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

  go (N n)    = (3, fromIntegral (error "fromEnum" n*2))
  go (x :@ y) = (rBits ∷ Int, rNum ∷ Natural)
   where
    ((xBits, xNum), (yBits, yNum)) = (go x, go y)
    rBits = 1 + xBits + yBits
    rNum  = 1 .|. shiftL xNum 1 .|. shiftL yNum (1 + xBits)
