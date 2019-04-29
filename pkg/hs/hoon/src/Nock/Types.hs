module Nock.Types where

import ClassyPrelude

type Atom = Int

data Noun = Atom !Atom
          | Cell !Noun !Noun
  deriving (Eq, Ord, Read, Show)

yes, no :: Noun
yes = Atom 0
no  = Atom 1

-- | Tree address
type Axis = Atom

data Nock = NCons Nock Nock  -- ^ autocons
          | NZeroAxis Axis   -- ^ 0: tree addressing
          | NOneConst Noun
          | NTwoCompose Nock Nock
          | NThreeIsCell Nock
          | NFourSucc Nock
          | NFiveEq Nock Nock
          | NSixIf Nock Nock Nock
          | NSevenThen Nock Nock
          | NEightPush Nock Nock
          | NNineInvoke Axis Nock
          | NTenEdit (Axis, Nock) Nock
          | NElevenHint Hint Nock
          | NTwelveScry Nock Nock
  deriving (Eq, Ord, Read, Show)

data Hint = Tag Atom
          | Assoc Atom Nock
  deriving (Eq, Ord, Read, Show)

nockToNoun :: Nock -> Noun
nockToNoun = go
  where
    go (NCons n m)         = (Cell (go n) (go m))
    go (NZeroAxis a)       = (Cell (Atom 0) (Atom a))
    go (NOneConst c)       = (Cell (Atom 1) c)
    go (NTwoCompose n m)   = (Cell (Atom 2) (Cell (go n) (go m)))
    go (NThreeIsCell n)    = (Cell (Atom 3) (go n))
    go (NFourSucc n)       = (Cell (Atom 4) (go n))
    go (NFiveEq n m)       = (Cell (Atom 5) (Cell (go n) (go m)))
    go (NSixIf n m o)      = (Cell (Atom 6) (Cell (go n) (Cell (go m) (go o))))
    go (NSevenThen n m)    = (Cell (Atom 7) (Cell (go n) (go m)))
    go (NEightPush n m)    = (Cell (Atom 8) (Cell (go n) (go m)))
    go (NNineInvoke a n)   = (Cell (Atom 9) (Cell (Atom a) (go n)))
    go (NTenEdit (a, n) m) = (Cell (Atom 10) (Cell (Cell (Atom a) (go n)) (go m)))
    go (NElevenHint h n)   = (Cell (Atom 11) (Cell (ho h) (go n)))
    go (NTwelveScry n m)   = (Cell (Atom 12) (Cell (go n) (go m)))

    ho (Tag x) = (Atom x)
    ho (Assoc x n) = (Cell (Atom x) (go n))

