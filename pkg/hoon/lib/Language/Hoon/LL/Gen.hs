module Language.Hoon.LL.Gen where

import ClassyPrelude hiding (union)
import Data.Bits (shift, finiteBitSize, countLeadingZeros)

import Language.Hoon.IR.Ty (Sym, Nat)
import Language.Hoon.LL.Types
import Language.Hoon.Nock.Types

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Vector as Vector

axis :: LLPath -> Atom
axis = go . reverse
  where
    go [] = 1
    go (L:ds) = 2 * go ds
    go (R:ds) = 2 * go ds + 1
    go (Ctx:ds) = 2 * go ds + 1

log2 :: Int -> Int
log2 x = finiteBitSize x - 1 - countLeadingZeros x

-- | The greatest power of two less than the argument
scale :: Int -> Int
scale x = shift (log2 x) 1

layoutPath :: Nat -> Nat -> LLPath
layoutPath 0 1 = []
layoutPath _ 1 = error "axis-fell"
layoutPath idx sz =
  let midpoint = sz `quot` 2
  in if idx <= midpoint
    then L : layoutPath idx midpoint
    else R : layoutPath (idx - midpoint) (sz - midpoint)

-- | The axis of an arm, as laid out in a battery
coreAxis :: Sym -> (Map Sym t) -> Maybe Atom
coreAxis a bat = do
  i <- Map.lookupIndex a bat
  let batPath = layoutPath i (Map.size bat)
  pure (axis (L : batPath))

layOut :: Vector Noun -> Noun
layOut ns
  | null ns        = Atom 0
  | length ns == 1 = Vector.head ns
  | otherwise      = Cell (layOut as) (layOut bs)
  where (as, bs) = splitAt (length ns `quot` 2) ns

battery :: (Map Sym a) -> (a -> Maybe Noun) -> Maybe Noun
battery bat conv = layOut <$> Vector.fromList <$> Map.elems <$> traverse conv bat

generate :: LLTy -> Maybe Nock
generate = \case
  LWith _ x (LFire _ s bat) -> do
    ax <- coreAxis s bat
    x' <- generate x
    -- FIXME icky
    pure (NNineInvoke ax x')
  LWith _ x y -> NSevenThen <$> generate x <*> generate y
  LAxis _ a -> pure (NZeroAxis (axis a))
  LEdit _ a r x -> do
    r' <- generate r
    x' <- generate x
    pure (NTenEdit (axis a, r') x')
  LFire _ s bat -> do
    ax <- coreAxis s bat
    -- FIXME icky
    pure (NNineInvoke ax (NZeroAxis 1))
  LAtom _ n -> pure (NOneConst (Atom n))
  LPair _ x y -> NCons <$> generate x <*> generate y
  LCore _ arms -> do
    b <- battery arms (fmap nockToNoun . generate)
    pure (NCons (NOneConst b) (NZeroAxis 1))
  LSucc _ x -> NFourSucc <$> generate x
  LTest _ x y z -> NSixIf <$> generate x <*> generate y <*> generate z
  LCelQ _ x -> NThreeIsCell <$> generate x
  LEqlQ _ x y -> NFiveEq <$> generate x <*> generate y
