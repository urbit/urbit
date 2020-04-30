module Urlicht.Unify where

import ClassyPrelude

import Bound
import Bound.Name
import Bound.Scope
import Control.Monad.State.Strict
import Data.Void

import Urlicht.Core
import Urlicht.Elab
import Urlicht.Errors
import Urlicht.Meta

-- =/ id/<|a/# a a|>  <a/# x/a a>
-- id _ 1
--
-- id (M id) 1
--   M id ~ @
--   M := \id. @
-- id ((\id. @) id) i
-- id @ 1

unify :: Eq a => Value a -> Value a -> Elab ()
unify = go where
  go :: Eq a => Value a -> Value a -> Elab ()
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
solve :: Eq a => Meta -> [Value a] -> Value a -> Elab ()
solve m vs rhs = do
  xs <- checkSpine vs
  let v = mkSolution m xs rhs
  v <- scopeCheck v
  v <- occursCheck m v
  bindMeta m v

checkSpine :: [Value a] -> Elab [a]
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
scopeCheck :: Value a -> Elab (Value Void)
scopeCheck = maybe (report EScope) pure . closed

-- | The solution for a metavariable should not refer to that metavariable.
-- A more sophisticated strategy for handling such self-reference might
-- instead synthesize a recursor for non-suspicious self-referential terms,
-- e.g. to infer a recursive type. It would be interesting, but also maybe
-- frightening, to see if we can infer recursive algorithms too. Anyway, this
-- is why the function returns Elab (Value a) rather than Elab ().
occursCheck :: Meta -> Value a -> Elab (Value a)
occursCheck m v = do
  forMetas_ v \n -> if m == n then report EOccurs else pure ()
  pure v
