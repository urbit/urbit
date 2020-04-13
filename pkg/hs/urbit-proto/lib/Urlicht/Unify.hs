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

unify :: Eq a => Value a -> Value a -> Elab ()
unify = go where
  go :: Eq a => Value a -> Value a -> Elab ()
  go u v = do
    (,) <$> incorporateHead u <*> incorporateHead v >>= \case
      (VVAp x us, VVAp y vs) | x == y -> zipWithM_ go us vs  -- Ulf more cplx?
      (VMAp m us, VMAp n vs) | m == n -> zipWithM_ go us vs
      -- the famous pattern rule
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


-- | If the root of the expression has at its head a meta whose binding we
-- know, substitute it and evaluate.
--
-- Maybe we don't need this and can just use incorporate because of laziness
-- FIXME.
incorporateHead :: Value a -> Elab (Value a)
incorporateHead = go where
  go = \case
    v@(VMAp m vs) -> lookupMeta m >>= \case
      Just v -> go (vApps v vs)
      Nothing -> pure v
    v -> pure v

incorporate :: Value a -> Elab (Value a)
incorporate = go where
  go :: Value a -> Elab (Value a)
  go = incorporateHead >=> \case
    VVAp x vs  -> VVAp x <$> traverse go vs
    VMAp m vs  -> VMAp m <$> traverse go vs
    VTyp       -> pure VTyp
    VFun v sv  -> VFun <$> go v <*> transverseScope go sv
    VLam sv    -> VLam <$> transverseScope go sv

solve :: Eq a => Meta -> [Value a] -> Value a -> Elab ()
solve m vs rhs = do
  xs <- checkSpine vs
  let v = mkSolution m xs rhs
  v <- scopeCheck v
  v <- occursCheck m v
  bindMeta m v

checkSpine :: [Value a] -> Elab [a]
checkSpine vs = for vs (incorporateHead >=> chk)
  where
    chk = \case
      VVar x -> pure x
      _ -> report ESpine

-- TODO
--   - linearity check?
--   - Kovacs: "bad to use normal forms in real impls because of size explo"
mkSolution :: Eq a => Meta -> [a] -> Value a -> Value a
mkSolution m xs v = foldl' step v xs
  where
    step term x = VLam $ mapBound (Name "?") -- TODO
                       $ abstract1 x term

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
