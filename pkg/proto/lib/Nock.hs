module Nock where

import ClassyPrelude

type Atom = Integer

type Noun = Tree Atom
data Tree a  = A !a
             | C !(Tree a) !(Tree a)
  deriving (Eq, Ord, Read, Functor)

data Fern a = FernA !a
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

-- | Tree address
type Axis = Atom

data Nock = NC Nock Nock           -- ^ ^: autocons
          | N0 Axis                -- ^ 0, axis: tree addressing
          | N1 Noun                -- ^ 1, const: constant
          | N2 Nock Nock           -- ^ 2, compose: compute subject, formula; apply
          | N3 Nock                -- ^ 3, is cell
          | N4 Nock                -- ^ 4, succ
          | N5 Nock Nock           -- ^ 5, eq
          | N6 Nock Nock Nock      -- ^ 6, if
          | N7 Nock Nock           -- ^ 7, then: =>
          | N8 Nock Nock           -- ^ 8, push: =+
          | N9 Axis Nock           -- ^ 9, invoke
          | N10 (Axis, Nock) Nock  -- ^ 10, edit
          | N11 Hint Nock          -- ^ 11, hint
          | N12 Nock Nock          -- ^ 12, scry
  deriving (Eq, Ord, Read)

data Hint = Tag Atom
          | Assoc Atom Nock
  deriving (Eq, Ord, Read, Show)

instance Show Nock where
  show = show . nockToNoun

nockToNoun :: Nock -> Noun
nockToNoun = go
  where
    go = \case
      NC f g -> C (go f) (go g)
      N0 a -> C (A 0) (A a)
      N1 n -> C (A 1) n
      N2 f g -> C (A 2) (C (go f) (go g))
      N3 f -> C (A 3) (go f)
      N4 f -> C (A 4) (go f)
      N5 f g -> C (A 5) (C (go f) (go g))
      N6 f g h -> C (A 5) (C (go f) (C (go g) (go h)))
      N7 f g -> C (A 7) (C (go f) (go g))
      N8 f g -> C (A 8) (C (go f) (go g))
      N9 a f -> C (A 9) (C (A a) (go f))
      N10 (a, f) g -> C (A 10) (C (C (A a) (go f)) (go g))
      N11 (Tag a) f -> C (A 11) (C (A a) (go f))
      N11 (Assoc a f) g -> C (A 11) (C (C (A a) (go f)) (go g))
      N12 f g -> C (A 12) (C (go f) (go g))

nounToNock :: Noun -> Nock
nounToNock = go
  where
    go = \case
      A{} -> error "nounToNock: atom"
      C n@C{} m -> NC (go n) (go m)
      C (A op) n -> case op of
        0  | (A a)              <- n  -> N0 a
        1                             -> N1 n
        2  | (C m o)            <- n  -> N2 (go m) (go o)
        3                             -> N3 (go n)
        4                             -> N4 (go n)
        5  | (C m o)            <- n  -> N5 (go m) (go o)
        6  | (C m (C o p))      <- n  -> N6 (go m) (go o) (go p)
        7  | (C m o)            <- n  -> N7 (go m) (go o)
        8  | (C m o)            <- n  -> N8 (go m) (go o)
        9  | (C (A a) m)        <- n  -> N9 a (go m)
        10 | (C (C (A a) m) o)  <- n  -> N10 (a, (go m)) (go o)
        11 | (C (C (A a) m) o)  <- n  -> N11 (Assoc a (go m)) (go o)
           | (C (A a) m)        <- n  -> N11 (Tag a) (go m)
        12 | (C m o)            <- n  -> N12 (go m) (go o)
        _ -> error ("nockToNoun: invalid " <> show op <> " " <> show n)

-- | Nock interpreter
nock :: Noun -> Nock -> Noun
nock n = \case
  NC f g -> C (nock n f) (nock n g)
  N0 a -> axis a n
  N1 n' -> n'
  N2 sf ff -> nock (nock n sf) (nounToNock (nock n ff))
  N3 f -> case nock n f of
    C{} -> yes
    A{} -> no
  N4 f -> case nock n f of
    C{} -> error "nock: cannot increment cell"
    A a -> A (a + 1)
  N5 f g -> if nock n f == nock n g then yes else no
  N6 f g h -> case nock n f of
    (A 0) -> nock n g
    (A 1) -> nock n h
    _ -> error "nock: invalid test value"
  N7 f g -> nock (nock n f) g
  N8 f g -> nock (C (nock n f) n) g
  N9 a f -> let c = nock n f in nock c (nounToNock (axis a c))
  N10{} -> error "nock: I don't want to implement editing right now"
  N11 _ f -> nock n f
  N12{} -> error "nock: scrying is not allowed"


-- Axis logic

data Dir = L | R
  deriving (Eq, Ord, Enum, Read, Show)
type Path = [Dir]

-- Write an axis as a binary number; e.g. 5 as 101.
-- The rule is: after droping the 1 in the msb, you read from left to right.
-- 0 becomes L and 1 becomes R.

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
axis a n = error ("bad axis: " ++ show a)

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
