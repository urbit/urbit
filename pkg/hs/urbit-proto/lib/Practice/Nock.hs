module Practice.Nock
  ( Nock
  , pattern NC
  , pattern N0
  , pattern N1
  , pattern N2
  , pattern N3
  , pattern N4
  , pattern N5
  , pattern N6
  , pattern N7
  , pattern N8
  , pattern N9
  , pattern N10
  , pattern N11Tag
  , pattern N11Run
  , pattern N12
  , nock
  , module NounStuff
  ) where

import ClassyPrelude

import SimpleNoun

import SimpleNoun as NounStuff (Noun, Tree(..))

type Nock = Noun

-- | ^: autocons
pattern NC :: Nock -> Nock -> Nock
pattern NC n m = C n m

-- | 0, axis; tree addressing
pattern N0 :: Axis -> Nock
pattern N0 a = C (A 0) (A a)

-- | 1, const
pattern N1 :: Noun -> Nock
pattern N1 n = C (A 1) n

-- | 2, compose; compute subject, formula: .*
pattern N2 :: Nock -> Nock -> Nock
pattern N2 n m = C (A 2) (C n m)

-- | 3, is cell: .?
pattern N3 :: Nock -> Nock
pattern N3 n = C (A 3) n

-- | 4, succ: .+
pattern N4 :: Nock -> Nock
pattern N4 n = C (A 4) n

-- | 5, eq: .=
pattern N5 :: Nock -> Nock -> Nock
pattern N5 n m = C (A 5) (C n m)

-- | 6, if: ?:
pattern N6 :: Nock -> Nock -> Nock -> Nock
pattern N6 n m o = C (A 6) (C n (C m o))

-- | 7, with: =>
pattern N7 :: Nock -> Nock -> Nock
pattern N7 n m = C (A 7) (C n m)

-- | 8, push: =+
pattern N8 :: Nock -> Nock -> Nock
pattern N8 n m = C (A 8) (C n m)

-- | 9, invoke
pattern N9 :: Axis -> Nock -> Nock
pattern N9 a n = C (A 9) (C (A a) n)

-- | 10, edit: %=
pattern N10 :: Axis -> Nock -> Nock -> Nock
pattern N10 a n m = C (A 10) (C (C (A a) n) m)

-- | 11, hint (without computed data)
pattern N11Tag :: Atom -> Nock -> Nock
pattern N11Tag a m   = C (A 11) (C (A a)       m)

-- | 11, hint (with computed data)
pattern N11Run :: Atom -> Nock -> Nock -> Nock
pattern N11Run a n m = C (A 11) (C (C (A a) n) m)

-- | 12, scry
pattern N12 :: Nock -> Nock -> Nock
pattern N12 n m = C (A 12) (C n m)


-- Interpreter -----------------------------------------------------------------

-- | Interpret the given nock formula against the given subject noun.
nock :: Noun -> Nock -> Noun
nock n = \case
  NC f g -> C (nock n f) (nock n g)
  N0 a -> axis a n
  N1 m -> m
  N2 sf ff -> nock (nock n sf) (nock n ff)
  N3 f -> case nock n f of
    C{} -> yes
    A{} -> no
  N4 f -> case nock n f of
    C{} -> error "nock: cannot increment cell"
    A a -> A (a + 1)
  N5 f g -> loob (nock n f == nock n g)
  N6 f g h -> case nock n f of
    A 0 -> nock n g
    A 1 -> nock n h
    _ -> error "nock: invalid test value"
  N7 f g -> nock (nock n f) g
  N8 f g -> nock (C (nock n f) n) g
  N9 a f -> let c = nock n f in nock c (axis a c)
  N10 a f g -> edit a (nock n f) (nock n g)
  N11Tag _ f -> nock n f
  N11Run _ _ f -> nock n f
  N12{} -> error "nock: scrying is not allowed yet"
  _ -> error "nock: misnock"
