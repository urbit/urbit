module Urbit.Urlicht.Unify where

import ClassyPrelude

import Bound
import Control.Monad.State.Strict
import Data.Void

import Urbit.Urlicht.Core
import Urbit.Urlicht.Elab
import Urbit.Urlicht.Errors
import Urbit.Urlicht.Meta

-- =/ id/<|a/# a a|>  <a/# x/a a>
-- id _ 1
--
-- id (M id) 1
--   M id ~ @
--   M := \id. @
-- id ((\id. @) id) i
-- id @ 1

unify :: (Eq a, Elab m) => Value a -> Value a -> m ()
unify u v = go u v where
  go :: (Eq a, Elab m) => Value a -> Value a -> m ()
  go u v = do
    (,) <$> crank u <*> crank v >>= \case
      (VVAp x us, VVAp y vs) | x == y -> zipWithM_ go us vs  -- Ulf more cplx?
      (VMAp m us, VMAp n vs) | m == n -> zipWithM_ go us vs
      -- the famous pattern rule
      -- M x y ~ v means add the following entry in metas: M := \x y. v
      -- M is a metavariable applied to x and y, which must be normal variables;
      -- v can be any value (provided it doesn't refer to M)
      (VMAp m us, v) -> solve m us v
      (u, VMAp m vs) -> solve m vs u
      --
      (VTyp, VTyp) -> pure ()
      (VLam su, VLam sv) -> go (fromScope su) (fromScope sv)
      -- TODO eta conversion checking? seems ill-typed
      (VFun s su, VFun t sv) -> do
        go s t
        go (fromScope su) (fromScope sv)
      _ -> report EUnify

-- | Try to unify (M vs) with rhs.
solve :: (Eq a, Elab m) => Meta -> [Value a] -> Value a -> m ()
solve m vs rhs = do
  xs <- checkSpine vs
  let v = mkSolution m xs rhs
  v <- scopeCheck v
  v <- occursCheck m v
  bindMeta m v

checkSpine :: Elab m => [Value a] -> m [a]
checkSpine vs = for vs (crank >=> chk) where
  chk = \case
    VVar x -> pure x
    _ -> report ESpine

-- | For the purpose of unifying (M x y) with v, construct the lambda expression
-- \x y. v.
-- TODO
--   - linearity check?
--   - Kovacs: "bad to use normal forms in real impls because of size explo"
mkSolution :: Eq a => Meta -> [a] -> Value a -> Value a
mkSolution m xs v = foldl' step v xs
  where
    step term x = VLam $ abstract1 x term

-- | When unifying (M x y) ~ v, make sure x and y are the only free vars in v.
-- This actually checks that \x y. v has no free vars.
scopeCheck :: Elab m => Value a -> m (Value Void)
scopeCheck = maybe (report EScope) pure . closed

-- | The solution for a metavariable should not refer to that metavariable.
-- A more sophisticated strategy for handling such self-reference might
-- instead synthesize a recursor for non-suspicious self-referential terms,
-- e.g. to infer a recursive type. It would be interesting, but also maybe
-- frightening, to see if we can infer recursive algorithms too. Anyway, this
-- is why the function returns m (Value a) rather than m ().
occursCheck :: Elab m => Meta -> Value a -> m (Value a)
occursCheck m v = do
  forMetas_ v \n -> if m == n then report EOccurs else pure ()
  pure v
