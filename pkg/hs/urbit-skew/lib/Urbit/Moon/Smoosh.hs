{-# OPTIONS_GHC -Wall -Werror #-}

{- |
  This converts back and forth between super combinators represented as a
  series of nested lambdas and super combinators represented as a single
  lambda with `Nat` indexing to referer to super-combinator parameters.

  This is useful for optimizing the strictness transformation
  (`MakeStrict`) since we know that all of the arguments to a Jet will
  be passed in at the same time.
-}

module Urbit.Moon.Smoosh (smoosh, unSmoosh) where

import ClassyPrelude hiding (try, Prim)

import Bound.Scope        (fromScope, toScope)
import Bound.Var          (Var(..), unvar)
import Numeric.Natural    (Natural)
import Urbit.Moon.Bracket (Exp(..))


-- Types -----------------------------------------------------------------------

type Nat = Natural


-- Utils -----------------------------------------------------------------------

doTimes :: Nat -> (a -> a) -> a -> a
doTimes 0 _ z = z
doTimes n f z = doTimes (pred n) f (f z)

doTimesM :: Monad m => Nat -> (a -> m a) -> a -> m a
doTimesM 0 _ z = pure z
doTimesM n f z = f z >>= doTimesM (pred n) f


-- Smoosh ----------------------------------------------------------------------

smoosh :: Nat -> Exp p () a -> Maybe (Exp p () (Var Nat a))
smoosh 0    = const Nothing
smoosh topN =
  \expr -> initial expr >>= doTimesM (pred topN) iter
 where
  initial :: Exp p () a -> Maybe (Exp p () (Var Nat a))
  initial = fromLam >=> pure . fmap (unvar (B . const 0) F)

  iter :: Exp p () (Var Nat a) -> Maybe (Exp p () (Var Nat a))
  iter = fromLam >=> pure . fmap (unvar (B . const 0) (unvar (B . (+1)) F))

  fromLam :: Exp p () a -> Maybe (Exp p () (Var () a))
  fromLam = \case
    Lam () x -> pure (fromScope x)
    _        -> Nothing


-- UnSmoosh --------------------------------------------------------------------

unSmoosh :: Nat -> Exp p () (Var Nat a) -> Exp p () a
unSmoosh 0 _ = error "unSmoosh: bad arity"
unSmoosh topN x = final $ doTimes (topN - 1) iter x
 where
  final :: Exp p () (Var Nat a) -> Exp p () a
  final = Lam () . toScope . fmap (unvar (B . const ()) F)

  iter :: Exp p () (Var Nat a) -> Exp p () (Var Nat a)
  iter = Lam () . toScope . fmap (unvar unwrap (F . F))

  unwrap :: Nat -> Var () (Var Nat a)
  unwrap 0 = B ()
  unwrap n = F (B (n-1))
