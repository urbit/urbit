module DH3PropertyTests where

import ClassyPrelude

import Data.Bits
import Test.SmallCheck.Series
import Test.Tasty
import Test.Tasty.SmallCheck

import Practice.HoonCommon
import Practice.DependentHoon3
import Practice.TopLevelDH3

-- I wanted to test a bunch of laws for nest and eval/loft, but smallcheck wound
-- up taking too long even for extraordinarily shallow depths. Implication tests
-- were brutal, especially.

tests :: TestTree
tests = testGroup "DependentHoon3 property tests (small)"
  [ testProperty "fits reflexivity"  $ changeDepth (const 2) fitsRefl
  , testProperty "fits transitivity" $ changeDepth (const 1) fitsTrans
  -- , testProperty "fits same -> nest" $ changeDepth (const 2) fitsSameNest
  -- , testProperty "fits same -> cast" $ changeDepth (const 2) fitsSameCast
  -- , testProperty "fits nest -> cast" $ changeDepth (const 2) fitsNestCast
  ]

c :: Check () -> Bool
c x = case runReaderT x [] of
  Left{}  -> False
  Right{} -> True

fitsRefl = forAll \fit t -> c (fits @() fit t t)

fitsTrans = forAll \fit t u v ->
  c (fits @() fit t u) && c (fits fit u v) ==> c (fits fit t v)

fitsSameNest = forAll @Identity \t u ->
  c (fits @() FitSame t u) ==> c (fits FitNest t u)

fitsSameCast = forAll @Identity \t u ->
  c (fits @() FitSame t u) ==> c (fits FitCast t u)

fitsNestCast = forAll @Identity \t u ->
  c (fits @() FitNest t u) ==> c (fits FitCast t u)


--
-- Instances
--

instance (Ord a, Serial m a) => Serial m (Set a) where
  series = setFromList <$> series

instance (Ord a, Serial m a, Serial m b) => Serial m (Map a b) where
  series = mapFromList <$> series

instance Monad m => Serial m Term where
  series = generate $ \n -> take (1+n)
    [ "a"
    , "b"
    , "c"
    , "foo"
    , "bar"
    , "baz"
    ]

instance Monad m => Serial m Fit
instance Monad m => Serial m Grit
instance Monad m => Serial m Face
instance Serial m a => Serial m (Code a)
instance Serial m a => Serial m (Base a)

instance Serial m a => Serial m (Hop a b) where
  series = New <$> series

instance Monad m => Serial m Stub where
  series = Leg <$> (+ 1) <$> series

instance Monad m => Serial m Rump where
  series = do
    lvl <- localDepth log2 series
    axe <- (+ 1) <$> series
    pure $ Leg' (lvl, axe)
   where
    log2 i = finiteBitSize i - countLeadingZeros i
