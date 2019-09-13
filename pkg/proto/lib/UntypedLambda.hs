module UntypedLambda where

import ClassyPrelude

import Bound
import Control.Monad.State
import Data.Deriving (deriveEq1, deriveOrd1, deriveRead1, deriveShow1)
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Data.Void

import Nock

type Nat = Int

data Exp a
  = Var a
  | App (Exp a) (Exp a)
  | Lam (Scope () Exp a)
  deriving (Functor, Foldable, Traversable)

deriveEq1   ''Exp
deriveOrd1  ''Exp
deriveRead1 ''Exp
deriveShow1 ''Exp
makeBound ''Exp

deriving instance Eq a => Eq (Exp a)
deriving instance Ord a => Ord (Exp a)
deriving instance Read a => Read (Exp a)
deriving instance Show a => Show (Exp a)

lam :: Eq a => a -> Exp a -> Exp a
lam v e = Lam (abstract1 v e)

eval :: Exp a -> Exp a
eval = \case
  e@Var{} -> e
  e@Lam{} -> e
  (App e f) -> case eval e of
    (Lam s) -> instantiate1 (eval f) s
    e' -> (App e' (eval f))

-- 6, 30, 126, 510, ...
oldDeBruijn :: Nat -> Axis
oldDeBruijn = toAxis . go
  where
    go = \case
      0 -> [R,L]
      n -> [R,R] ++ go (n - 1)

-- | Raw de Bruijn
data Exp'
  = Var' Nat
  | App' Exp' Exp'
  | Lam' Exp'
  deriving (Eq, Ord, Read, Show)

toExp' :: Exp a -> Exp'
toExp' = go \v -> error "toExp': free variable"
  where
    go :: (a -> Nat) -> Exp a -> Exp'
    go env = \case
      Var v   -> Var' (env v)
      App e f -> App' (go env e) (go env f)
      Lam s   -> Lam' (go env' (fromScope s))
        where
          env' = \case
            B () -> 0
            F v  -> 1 + env v

-- | The old calling convention; i.e., what the (%-, |=) sublanguage of hoon
-- compiles to
old :: Exp a -> Nock
old = go \v -> error "old: free variable"
  where
    go :: (a -> Path) -> Exp a -> Nock
    go env = \case
      Var v   -> N0 (toAxis (env v))
      App e f -> app (go env e) (go (\v -> R : env v) f)
      Lam s   -> lam (nockToNoun (go env' (fromScope s)))
        where
          env' = \case
            B () -> [R,L]
            F v  -> [R,R] ++ env v
    app ef ff =
      N8
        ef  -- =+ callee so we don't modify the orig's bunt
        (N9 2
          (N10 (6, ff)
            (N0 2)))
    lam ff =
      N8  -- pushes onto the context
        (N1 (A 0))  -- a bunt value (in hoon, actually depends on type)
        (NC  -- then conses (N8 would also work, but hoon doesn't)
          (N1 ff)  -- the battery (nock code)
          (N0 1))  -- onto the pair of bunt and context

data CopyVar
  = Argument
  | Lexical Nat

data CopyExp
  = CVar CopyVar
  | CApp CopyExp CopyExp
  | CLam [CopyVar] CopyExp

toCopy :: forall a. Ord a => Exp a -> CopyExp
toCopy = fst . go \v -> error "toCopy: free variable"
  where
    go :: (a -> CopyVar) -> Exp a -> (CopyExp, Set a)
    go env = \case
      Var v -> (CVar (env v), singleton v)
      App e f -> (CApp ec fc, union eu fu)
        where
          (ec, eu) = go env e
          (fc, fu) = go env f
      Lam s -> (CLam (map env u) c, fromList u)
        where
          c :: CopyExp
          (c, u') = go env' (fromScope s)
          env' :: Var () a -> CopyVar
          env' = \case
            B () -> Argument
            F v  -> Lexical (fromJust (elemIndex v u))
          u = mapMaybe pred (toList u')
          pred = \case
            B () -> Nothing
            F v  -> Just v

copyToNock :: CopyExp -> Nock
copyToNock = \case
  CVar v -> toAxis case v of
    Argument -> [R]
    Lexical n -> L : repeat n R ++ L
  CApp e f -> N2 (copyToNock f) (copyToNock e)

-- | The proposed new calling convention
new :: Exp a -> Nock
new = go \v -> error "new: free variable"
  where
    go = undefined



-- x. y. x
-- old: [8 [1 0] [1 8 [1 0] [1 0 30] 0 1] 0 1]
--      =+  0  =
