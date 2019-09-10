module Nock where

import ClassyPrelude

type Atom = Integer

data Tree a  = Atom !a
             | Cell !(Tree a) !(Tree a)
  deriving (Eq, Ord, Read, Show, Functor)

type Noun = Tree Atom

yes, no :: Noun
yes = Atom 0
no  = Atom 1

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
  deriving (Eq, Ord, Read, Show)

data Hint = Tag Atom
          | Assoc Atom Nock
  deriving (Eq, Ord, Read, Show)

nockToNoun :: Nock -> Noun
nockToNoun = go
  where
    go = \case
      NC f g -> Cell (go f) (go g)
      N0 a -> Cell (Atom 0) (Atom a)
      N1 n -> Cell (Atom 1) n
      N2 f g -> Cell (Atom 2) (Cell (go f) (go g))
      N3 f -> Cell (Atom 3) (go f)
      N4 f -> Cell (Atom 4) (go f)
      N5 f g -> Cell (Atom 5) (Cell (go f) (go g))
      N6 f g h -> Cell (Atom 5) (Cell (go f) (Cell (go g) (go h)))
      N7 f g -> Cell (Atom 7) (Cell (go f) (go g))
      N8 f g -> Cell (Atom 8) (Cell (go f) (go g))
      N9 a f -> Cell (Atom 9) (Cell (Atom a) (go f))
      N10 (a, f) g -> Cell (Atom 10) (Cell (Cell (Atom a) (go f)) (go g))
      N11 (Tag a) f -> Cell (Atom 11) (Cell (Atom a) (go f))
      N11 (Assoc a f) g -> Cell (Atom 11) (Cell (Cell (Atom a) (go f)) (go g))
      N12 f g -> Cell (Atom 12) (Cell (go f) (go g))

nounToNock :: Noun -> Nock
nounToNock = go
  where
    go = \case
      Atom{} -> error "nounToNock: atom"
      Cell n@Cell{} m -> NC (go n) (go m)
      Cell (Atom op) n -> case op of
        0  | (Atom a)                   <- n  -> N0 a
        1                                     -> N1 n
        2  | (Cell m o)                 <- n  -> N2 (go m) (go o)
        3                                     -> N3 (go n)
        4                                     -> N4 (go n)
        5  | (Cell m o)                 <- n  -> N5 (go m) (go o)
        6  | (Cell m (Cell o p))        <- n  -> N6 (go m) (go o) (go p)
        7  | (Cell m o)                 <- n  -> N7 (go m) (go o)
        8  | (Cell m o)                 <- n  -> N8 (go m) (go o)
        9  | (Cell (Atom a) m)          <- n  -> N9 a (go m)
        10 | (Cell (Cell (Atom a) m) o) <- n  -> N10 (a, (go m)) (go o)
        11 | (Cell (Cell (Atom a) m) o) <- n  -> N11 (Assoc a (go m)) (go o)
           | (Cell (Atom a) m)          <- n  -> N11 (Tag a) (go m)
        12 | (Cell m o)                 <- n  -> N12 (go m) (go o)
        _ -> error ("nockToNoun: invalid " <> show op <> " " <> show n)

-- | Nock interpreter
nock :: Noun -> Nock -> Noun
nock n = \case
  NC f g -> Cell (nock n f) (nock n g)
  N0 a -> axis a n
  N1 n' -> n'
  N2 sf ff -> nock (nock n sf) (nounToNock (nock n ff))
  N3 f -> case nock n f of
    Cell{} -> yes
    Atom{} -> no
  N4 f -> case nock n f of
    Cell{} -> error "nock: cannot increment cell"
    Atom a -> Atom (a + 1)
  N5 f g -> if nock n f == nock n g then yes else no
  N6 f g h -> case nock n f of
    (Atom 0) -> nock n g
    (Atom 1) -> nock n h
    _ -> error "nock: invalid test value"
  N7 f g -> nock (nock n f) g
  N8 f g -> nock (Cell (nock n f) n) g
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
axis (capMas -> (d, r)) (Cell n m) = case d of
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
