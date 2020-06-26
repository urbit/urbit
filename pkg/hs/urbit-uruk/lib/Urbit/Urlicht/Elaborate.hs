-- | Elaboration from Simple to Core using pattern unification and piggybacking
-- on bidirectional typechecking. TODO more helpful comment.

module Urbit.Urlicht.Elaborate where

import ClassyPrelude

import Bound
import Bound.Var
import Bound.Scope
import Control.Monad.Morph (hoist)

import Urbit.Urlicht.Core
import Urbit.Urlicht.Elab
import Urbit.Urlicht.Env
import Urbit.Urlicht.Errors
import qualified Urbit.Urlicht.Simple as S
import Urbit.Urlicht.Unify

-- | To elaborate a hole we create a new meta and apply it to all the vars
-- in scope. Kovacs is able to eliminate shadowed vars lol.
newMetaWithSpine :: Elab m => Env a -> m (Core a)
newMetaWithSpine env = do
  m <- freshMeta
  pure $ foldr (flip App) (Met m) (Var <$> unknowns env)

freshFun :: Elab m => Env a -> m (Type a, Scope B Type a)
freshFun env = do
  u <- evalIn env =<< newMetaWithSpine env
  let env' = bind env $ Fun' u
  v <- evalIn env' =<< newMetaWithSpine env'
  pure (u, toScope v)

evalIn :: Elab m => Env a -> Core a -> m (Value a)
evalIn env = go (scryVal env) where
  extend :: (a -> Maybe (Value a)) -> Var () a -> Maybe (Value (Var () a))
  extend look = \case
    B _ -> Nothing
    F v -> fmap F <$> look v
  go :: Elab m => (a -> Maybe (Value a)) -> Core a -> m (Value a)
  go look = \case
    Var x -> pure $ maybe (VVar x) id (look x)
    Met m -> maybe (VMet m) id <$> lookupMeta m
    --
    Typ -> pure VTyp
    Fun c sc -> do
      v <- go look c
      sv <- toScope <$> go (extend look) (fromScope sc)
      pure $ VFun v sv
    --
    Lam sc -> VLam <$> toScope <$> go (extend look) (fromScope sc)
    --
    App c d -> vApp <$> go look c <*> go look d
    --
    -- We could also add a mapping to rhs in look, but then we'd need to back
    -- out the result from Value (Var () a) to Value a, which is annoying.
    Let c t sc -> go look $ instantiate1 c sc

-- Gamma |- e : t
-- Gamma |- e -> t  inference  Gamma, e -> Maybe t
-- Gamma |- e <- t  checking   Gamma, e, t -> ?

check :: (Eq a, Elab m) => Env a -> S.Simple a -> Type a -> m (Core a)
check env simp ty = do
  ty <- crank ty
  let
    checkInfer = do
      (ty', c) <- infer env simp
      -- TODO better error
      unify ty ty'
      pure c
  case (simp, ty) of
    (S.Var{}, _) -> checkInfer
    (S.Met m, _) -> pure $ Met m
    (S.Typ{}, _) -> checkInfer
    (S.Fun{}, _) -> checkInfer
    (S.Lam ss, VFun t st) ->
      -- TODO avoid fromScope/toScope
      Lam . toScope <$>
        check (bind env $ Lam' t) (fromScope ss) (fromScope st)
    (S.Lam{}, _) -> checkInfer
    (S.App{}, _) -> checkInfer
    (S.Let tyRhs sRhs sBody, _) -> do
      cTyRhs <- check env tyRhs VTyp
      vTyRhs <- evalIn env cTyRhs
      cRhs <- check env sRhs vTyRhs
      vRhs <- evalIn env cRhs
      Let cTyRhs cRhs . toScope <$>
        check (bind env $ Let' vTyRhs vRhs) (fromScope sBody) (F <$> ty)
    (S.Hol, _) -> newMetaWithSpine env
    (S.Asc sExp sTy, _) -> do
      ty' <- evalIn env =<< check env sTy VTyp
      unify ty ty'
      check env sExp ty

infer :: (Eq a, Elab m) => Env a -> S.Simple a -> m (Type a, Core a)
infer env = \case
  S.Var x -> case scry env x of
    Just ty -> pure (binderTy ty, Var x)
    Nothing -> report EName
  S.Met m -> error "error: elaborating a raw metavariable"
  S.Typ -> pure (VTyp, Typ)
  S.Fun s ss -> do
    c <- check env s VTyp
    v <- evalIn env c
    sc <- toScope <$> check (bind env $ Fun' v) (fromScope ss) VTyp
    pure (VTyp, Fun c sc)
  S.Lam ss -> do
    (t, st) <- freshFun env
    c <- check env (S.Lam ss) (VFun t st)
    pure (VFun t st, c)
  S.App fun arg -> do
    (tFun, cFun) <- infer env fun
    (a, b) <- crank tFun >>= \case
      VFun a b -> pure (a, b)
      -- It's like Hindley-Milner
      tFun@VMAp{} -> do
        (a, b) <- freshFun env
        -- TODO better errors
        unify tFun (VFun a b)
        pure (a, b)
      _ -> report ENotFun
    cArg <- check env arg a
    -- TODO make succ less
    tRes <- evalIn env $ instantiate (const cArg) (hoist quote b)
    pure (tRes, App cFun cArg)
  S.Let tyRhs sRhs sBod -> do
    cTyRhs <- check env tyRhs VTyp
    vTyRhs <- evalIn env cTyRhs
    cRhs <- check env sRhs vTyRhs
    vRhs <- evalIn env cRhs
    (tyBod, cBod) <- infer (bind env $ Let' vTyRhs vRhs) (fromScope sBod)
    -- FIXME I guess evaluating with let rhs would be nice here
    let tyBod' = unvar (error "infer: let-bound var in value") id <$> tyBod
    pure (tyBod', Let cTyRhs cRhs (toScope cBod))
  S.Hol -> do
    t <- evalIn env =<< newMetaWithSpine env
    c <- newMetaWithSpine env
    pure (t, c)
  S.Asc exp typ -> do
    vTy <- evalIn env =<< check env typ VTyp
    exp <- check env exp vTy
    pure (vTy, exp)

-- | Inline metavariables without evaluating.
zonk :: Elab m => Core a -> m (Core a)
zonk = go where
  go :: Elab m => Core a -> m (Core a)
  go = \case
    v@Var{}  -> pure v
    Met m    -> lookupMeta m >>= \case
      Just v  -> go (quote v)
      Nothing -> pure (Met m)
    Typ      -> pure Typ
    Fun c sc -> Fun <$> go c <*> transverseScope go sc
    Lam sc   -> Lam <$> transverseScope go sc
    App c d  -> App <$> go c <*> go d
    Let t c sc -> Let <$> go t <*> go c <*> transverseScope go sc
