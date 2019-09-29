module Nock where

import ClassyPrelude

import Dashboard
import Noun

data Nock
  = NC Nock Nock           -- ^ ^: autocons
  | N0 Axis                -- ^ 0, axis: tree addressing
  | N1 Noun                -- ^ 1, const
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
  deriving (Eq, Ord, Read, Generic)

data Hint = Tag Atom
          | Assoc Atom Nock
  deriving (Eq, Ord, Read, Show, Generic)

instance Hashable Nock
instance Hashable Hint

instance Show Nock where
  show = show . nockToNoun

nockToNoun :: Nock -> Noun
nockToNoun = go
  where
    go = \case
      NC f g            -> C (go f) (go g)
      N0 a              -> C (A 0) (A a)
      N1 n              -> C (A 1) n
      N2 f g            -> C (A 2) (C (go f) (go g))
      N3 f              -> C (A 3) (go f)
      N4 f              -> C (A 4) (go f)
      N5 f g            -> C (A 5) (C (go f) (go g))
      N6 f g h          -> C (A 6) (C (go f) (C (go g) (go h)))
      N7 f g            -> C (A 7) (C (go f) (go g))
      N8 f g            -> C (A 8) (C (go f) (go g))
      N9 a f            -> C (A 9) (C (A a) (go f))
      N10 (a, f) g      -> C (A 10) (C (C (A a) (go f)) (go g))
      N11 (Tag a) f     -> C (A 11) (C (A a) (go f))
      N11 (Assoc a f) g -> C (A 11) (C (C (A a) (go f)) (go g))
      N12 f g           -> C (A 12) (C (go f) (go g))

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
nock :: (Dashboard d) => Noun -> Nock -> d Noun
nock n = \case
  NC f g -> C <$> nock n f <*> nock n g
  N0 a -> pure $ axis a n
  N1 n' -> pure n'
  N2 sf ff -> do
    s <- nock n sf
    f <- nock n ff
    match f >>= \case
      Just jet -> pure (jet s)
      Nothing  -> nock s (nounToNock f)
  N3 f -> nock n f <&> \case
    C{} -> yes
    A{} -> no
  N4 f -> nock n f <&> \case
    C{} -> error "nock: cannot increment cell"
    A a -> A (a + 1)
  N5 f g -> loob <$> ((==) <$> nock n f <*> nock n g)
  N6 f g h -> nock n f >>= \case
    (A 0) -> nock n g
    (A 1) -> nock n h
    _ -> error "nock: invalid test value"
  N7 f g -> do
    n' <- nock n f
    nock n' g
  N8 f g -> do
    n' <- nock n f
    nock (C n' n) g
  N9 a f -> do
    c <- nock n f
    nock c (nounToNock (axis a c))
  N10 (a, f) g -> edit a <$> nock n f <*> nock n g
  N11 _ f -> nock n f
  N12{} -> error "nock: scrying is not allowed"

