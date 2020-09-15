module Urbit.Urlicht.Core where

import ClassyPrelude

import Bound
import Control.Monad.Morph (hoist)
import Data.Deriving (deriveEq1, deriveOrd1, deriveRead1, deriveShow1)

import Urbit.Urlicht.Meta

type B = ()

data Core a
  = Var a
  | Met Meta
  -- types
  | Typ
  | Fun (Core a) (Scope B Core a)
  -- introduction forms
  | Lam (Scope B Core a)
  -- elimination forms
  | App (Core a) (Core a)
  -- flow control
  | Let (Core a) (Core a) (Scope B Core a)
  deriving (Functor, Foldable, Traversable)

deriveEq1   ''Core
deriveOrd1  ''Core
deriveRead1 ''Core
deriveShow1 ''Core
makeBound   ''Core

deriving instance Eq a   => Eq   (Core a)
deriving instance Ord a  => Ord  (Core a)
deriving instance Read a => Read (Core a)
deriving instance Show a => Show (Core a)

type Type = Value

-- | Terms in normal form. Kovacs distinguishes these, which is nice for type
-- safety, but also, supposedly, opens the way to faster performance with
-- better representations. For example, we might eventually do our compile-time
-- computation in Skew, so that Value is an Skew value, and use W to get it
-- back out.
data Value a
  = VVAp a [Value a]                  -- ^ free var applied to (reversed) args
  | VMAp Meta [Value a]               -- ^ metavar applied to (reversed) args
  | VTyp
  | VFun (Value a) (Scope B Value a)  -- TODO maybe just use Value (Var B a)?
  | VLam (Scope B Value a)
  deriving (Functor, Foldable, Traversable)

pattern VVar x = VVAp x []
pattern VMet m = VMAp m []

deriveEq1   ''Value
deriveOrd1  ''Value
deriveRead1 ''Value
deriveShow1 ''Value

deriving instance Eq a   => Eq   (Value a)
deriving instance Ord a  => Ord  (Value a)
deriving instance Read a => Read (Value a)
deriving instance Show a => Show (Value a)

instance Applicative Value where
  pure = VVar
  (<*>) = ap

instance Monad Value where
  return = VVar
  -- FIXME slow?
  v >>= f = eval $ (>>= (quote . f)) $ quote v

vApp :: Value a -> Value a -> Value a
vApp (VVAp x vs) v = VVAp x (v:vs)
vApp (VMAp m vs) v = VMAp m (v:vs)
-- TODO correct?
vApp (VLam   sv) v = instantiate1 v sv
vApp _           _ = error "vApp: applying non-function"

vApps :: Value a -> [Value a] -> Value a
vApps = foldr (flip vApp)

forMetas_ :: forall f a b. Monad f => Value a -> (Meta -> f b) -> f ()
forMetas_ v f = go v where
  go :: forall a. Value a -> f ()
  go = \case
    VVAp x vs -> traverse_ go vs
    VMAp m vs -> f m >> traverse_ go vs
    VTyp -> pure ()
    -- TODO don't use fromScope
    VFun v sv -> go v >> go (fromScope sv)
    VLam sv -> go (fromScope sv)

-- Kovacs does this relative to a metacontext, instantiating known vars; why?
-- is it just to manage substitution on recursion, or is this info needed from
-- the elaborator?
eval :: Core a -> Value a
eval = \case
  Var x -> VVar x
  Met m -> VMet m
  --
  Typ -> VTyp
  Fun c sc -> VFun (eval c) (hoist eval sc)
  --
  Lam sc -> VLam (hoist eval sc)
  --
  App c d -> vApp (eval c) (eval d)
  --
  Let c t sc -> eval $ instantiate1 c sc

-- Likewise with metacontext here?
quote :: Value a -> Core a
quote = go where
  go :: Value a -> Core a
  go = \case
    VVAp x vs -> foldr (\v acc -> App acc (go v)) (Var x) vs
    VMAp m vs -> foldr (\v acc -> App acc (go v)) (Met m) vs
    VTyp      -> Typ
    VFun v sv -> Fun (go v) (hoist go sv)
    VLam sv -> Lam (hoist go sv)
