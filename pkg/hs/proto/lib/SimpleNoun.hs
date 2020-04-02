module SimpleNoun where

import ClassyPrelude
import Numeric.Natural

import qualified Urbit.Noun as N

type Atom = Natural

type Noun = Tree Atom
data Tree a
  = A !a
  | C !(Tree a) !(Tree a)
  deriving (Eq, Ord, Read, Functor, Generic)

instance Hashable a => Hashable (Tree a)

data Fern a
  = FernA !a
  | FernF [Fern a]

toFern :: Tree a -> Fern a
toFern = \case
  A a -> FernA a
  C h t -> case toFern t of
    a@FernA{} -> FernF [toFern h, a]
    FernF fs -> FernF (toFern h : fs)

instance Show a => Show (Fern a) where
  show = \case
    FernA a -> show a
    FernF xs -> "[" <> intercalate " " (map show xs) <> "]"

instance Show a => Show (Tree a) where
  show = show . toFern

yes, no :: Noun
yes = A 0
no  = A 1

loob :: Bool -> Noun
loob = \case
  True  -> yes
  False -> no

textToAtom :: Text -> Atom
textToAtom t = case N.textToUtf8Atom t of
  N.A a -> a

showA :: Atom -> String
showA a = show (N.A a)

tshowA :: Atom -> Text
tshowA = pack . showA

-- | Tree address
type Axis = Atom

data Dir = L | R
  deriving (Eq, Ord, Enum, Read, Show)
type Path = [Dir]

-- some stuff from hoon.hoon

cap :: Axis -> Dir
cap = \case
  2 -> L
  3 -> R
  a | a <= 1    -> error "cap: bad axis"
    | otherwise -> cap (div a 2)

mas :: Axis -> Axis
mas = \case
  2 -> 1
  3 -> 1
  a | a <= 1    -> error "mas: bad axis"
    | otherwise -> (mod a 2) + 2 * mas (div a 2)

capMas :: Axis -> (Dir, Axis)
capMas = \case
  2 -> (L, 1)
  3 -> (R, 1)
  a | a <= 1    -> error "capMas: bad axis"
    | otherwise -> (d, (mod a 2) + 2 * r)
    where
      (d, r) = capMas (div a 2)

peg :: Axis -> Axis -> Axis
peg a = \case
  1 -> a
  2 -> a * 2
  3 -> a * 2 + 1
  b -> (mod b 2) + 2 * peg a (div b 2)

axis :: Axis -> Tree a -> Tree a
axis 1 n = n
axis (capMas -> (d, r)) (C n m) = case d of
  L -> axis r n
  R -> axis r m
axis a _ = error ("bad axis: " ++ show a)

edit :: Axis -> Tree a -> Tree a -> Tree a
edit 1 v n = v
edit (capMas -> (d, r)) v (C n m) = case d of
  L -> C (edit r v n) m
  R -> C n (edit r v m)
edit a _ _ = error ("bad edit: " ++ show a)

-- Write an axis as a binary number; e.g. 5 as 101.
-- The rule is: after droping the 1 in the msb, you read from left to right.
-- 0 becomes L and 1 becomes R. So 5 becomes [L,R]
toPath :: Axis -> Path
toPath = \case
  1 -> []
  (capMas -> (d, r)) -> d : toPath r

toAxis :: Path -> Axis
toAxis = foldl' step 1
  where
    step r = \case
      L -> 2 * r
      R -> 2 * r + 1
