module Urlicht.Elaborate where

-- | Elaboration from Simple to Core using pattern unification and piggybacking
-- on bidirectional typechecking. TODO more helpful comment.

import ClassyPrelude

import Bound
import Bound.Var
import Bound.Scope
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
type Env a = [(a, Type a)]

scry :: Eq a => Env a -> a -> Maybe (Type a)
scry = flip lookup

scope :: Env a -> [a]
scope = map fst

bind :: Env a -> Type a -> Env (Var () a)
bind bs t = (B (), fmap F t) : fmap (\(x, v) -> (F x, fmap F v)) bs

-- | To elaborate a hole we create a new meta and apply it to all the vars
-- in scope. Kovacs is able to eliminate shadowed vars lol.
--
-- TODO don't add let-bound vars to the spine.
newMetaWithSpine :: Elab m => Env a -> m (Core a)
newMetaWithSpine env = do
  m <- freshMeta
  pure $ foldr (flip App) (Met m) (Var <$> scope env)

freshFun :: Elab m => Env a -> m (Type a, Scope B Type a)
freshFun env = do
  a <- newMetaWithSpine env
  -- FIXME maybe we really do need to eval relative to a context with lets
  let v = eval a
  b <- newMetaWithSpine (bind env v)
  pure (v, toScope (eval b))

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
        check (bind env t) (fromScope ss) (fromScope st)
    (S.Lam{}, _) -> checkInfer
    (S.App{}, _) -> checkInfer
    (S.Let sRhs sBody, _) -> do
      -- TODO I guess put a type annotation in lets which can be Hol :/
      (tyRhs, cRhs) <- infer env sRhs
      Let cRhs . toScope <$>
        check (bind env tyRhs) (fromScope sBody) (F <$> ty)
    (S.Hol, _) -> newMetaWithSpine env
    (S.Asc sExp sTy, _) -> do
      ty' <- check env sTy VTyp
      unify ty (eval ty')
      check env sExp ty

infer :: (Eq a, Elab m) => Env a -> S.Simple a -> m (Type a, Core a)
infer env = \case
  S.Var x -> case scry env x of
    Just ty -> pure (ty, Var x)
    Nothing -> report EName
  S.Met m -> error "error: elaborating a raw metavariable"
  S.Typ -> pure (VTyp, Typ)
  S.Fun s ss -> do
    c <- check env s VTyp
    sc <- toScope <$> check (bind env (eval c)) (fromScope ss) VTyp
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
    let tRes = eval $ instantiate (const cArg) (hoist quote b)
    pure (tRes, App cFun cArg)
  S.Let sRhs sBod -> do
    (tyRhs, cRhs) <- infer env sRhs
    (tyBod, cBod) <- infer (bind env tyRhs) (fromScope sBod)
    -- I guess evaluating with let rhs would be nice here
    let tyBod' = eval $ quote tyBod >>= unvar (const cRhs) Var
    pure (tyBod', Let cRhs (toScope cBod))
  S.Hol -> do
    t <- newMetaWithSpine env
    c <- newMetaWithSpine env
    pure (eval t, c)
  S.Asc exp typ -> do
    typ <- check env typ VTyp
    let vTy = eval typ
    exp <- check env exp vTy
    pure (vTy, exp)

-- | Inline metavariables.
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
    Let c sc -> Let <$> go c <*> transverseScope go sc
