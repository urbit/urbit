module Urlicht.Elaborate where

-- | Elaboration from Simple to Core using pattern unification and piggybacking
-- on bidirectional typechecking. TODO more helpful comment.

import ClassyPrelude

import Bound
import Bound.Name
import Control.Monad.Morph (hoist)

import Urlicht.Core
import Urlicht.Elab
import Urlicht.Errors
import qualified Urlicht.Simple as S
import Urlicht.Unify

-- | Tracks the types of bound variables. It looks pretty inefficient, but
-- the other solutions I've come up with or seen so far are at least as bad.
-- Take succor in that it doesn't do worse than 141, well, except in bind.
-- But because of laziness, the cost is passed on to the consumer!
--
-- Kovacs (and I suppose Brady too) also stores the value associated with
-- a let binder, but for the life of me I can't figure out why. Does it
-- somehow give more specificity to unification solutions?
type Env a = [(a, ValueType a)]

scry :: Eq a => Env a -> a -> Maybe (ValueType a)
scry = flip lookup

scope :: Env a -> [a]
scope = map fst

bind :: Env a -> b -> ValueType a -> Env (Var b a)
bind bs b t = (B b, fmap F t) : fmap (\(x, v) -> (F x, fmap F v)) bs

nameHack = Name "??" ()

-- | To elaborate a hole we create a new meta and apply it to all the vars
-- in scope. Kovacs is able to eliminate shadowed vars lol.
metastasize :: Env a -> Elab (Core a)
metastasize env = do
  m <- freshMeta
  pure $ foldr (flip App) (Met m) (Var <$> scope env)

freshFun :: Env a -> Elab (ValueType a, Scope B ValueType a)
freshFun env = do
  a <- metastasize env
  undefined


