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
  | Atm Atom
  | Cel (Exp a) (Exp a)
  | IsC (Exp a)
  | Suc (Exp a)
  | Eql (Exp a) (Exp a)
  | Ift (Exp a) (Exp a) (Exp a)
  | Let (Exp a) (Scope () Exp a)
  | Jet Atom (Exp a)
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
  e@Atm{} -> e
  (Cel e f) -> Cel (eval e) (eval f)
  (IsC e) -> case eval e of
    Atm{} -> Atm 1
    Cel{} -> Atm 0
    Lam{} -> Atm 0  -- ehhhh
    Var{} -> error "eval: free variable"
    _ -> error "eval: implementation error"
  (Suc e) -> case eval e of
    Atm a -> Atm (a + 1)
    _ -> error "eval: cannot take successor of non-atom"
  (Ift e t f) -> case eval e of
    Atm 0 -> eval t
    Atm 1 -> eval f
    _ -> error "eval: not a boolean"
  (Let e s) -> instantiate1 (eval e) s
  Jet _ e -> eval e

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

cell :: Nock -> Nock -> Nock
cell (N1 n) (N1 m) = N1 (C n m)
cell ef ff = NC ef ff

-- | The old calling convention; i.e., what the (%-, |=) sublanguage of hoon
-- compiles to
old :: Exp a -> Nock
old = go \v -> error "old: free variable"
  where
    go :: (a -> Path) -> Exp a -> Nock
    go env = \case
      Var v     -> N0 (toAxis (env v))
      App e f   -> app (go env e) (go (\v -> R : env v) f)
      Lam s     -> lam (nockToNoun (go env' (fromScope s)))
        where
          env' = \case
            B () -> [R,L]
            F v  -> [R,R] ++ env v
      Cel e f   -> cell (go env e) (go env f)
      IsC e     -> N3 (go env e)
      Suc e     -> N4 (go env e)
      Eql e f   -> N5 (go env e) (go env f)
      Ift e t f -> N6 (go env e) (go env t) (go env f)
      Let e s   -> N8 (go env e) (go env' (fromScope s))
        where
          env' = \case
            B () -> [L]
            F v  -> R : env v
      Jet{}     -> error "Old-style jetting not supported"

    app ef ff =
      N8
        ef  -- =+ callee so we don't modify the orig's bunt
        (N9 2
          (N10 (6, ff)
            (N0 2)))
    lam ff =
      N8  -- pushes onto the context
        (N1 (A 0))  -- a bunt value (in hoon, actually depends on type)
        (NC  -- then celles (N8 would also work, but hoon doesn't)
          (N1 ff)  -- the battery (nock code)
          (N0 1))  -- onto the pair of bunt and context

data CopyVar
  = Argument
  | Lexical Nat

data CopyExp
  = CVar CopyVar
  | CApp CopyExp CopyExp
  | CLam [CopyVar] CopyExp
  | CAtm Atom
  | CCel CopyExp CopyExp
  | CIsC CopyExp
  | CSuc CopyExp
  | CEql CopyExp CopyExp
  | CIft CopyExp CopyExp CopyExp
  | CLet CopyExp CopyExp
  | CJet Atom CopyExp

toCopy :: Ord a => Exp a -> CopyExp
toCopy = fst . go \v -> error "toCopy: free variable"
  where
    go :: Ord a => (a -> CopyVar) -> Exp a -> (CopyExp, Set a)
    go env = \case
      Var v -> (CVar (env v), singleton v)
      App e f -> (CApp ec fc, union eu fu)
        where
          (ec, eu) = go env e
          (fc, fu) = go env f
      Lam s -> (CLam (map env u) c, setFromList u)
        where
          (c, u') = go env' (fromScope s)
          env' = \case
            B () -> Argument
            F v  -> Lexical (fromJust (elemIndex v u))
          u = mapMaybe pred (toList u')
          pred = \case
            B () -> Nothing
            F v  -> Just v

-- Possible improvements:
--   - store the copied values in a tree rather than list
--   - avoid a nock 8 if nothing is copied
copyToNock :: CopyExp -> Nock
copyToNock = go
  where
    go = \case
      CVar v -> N0 $ toAxis case v of
        -- FIXME let
        Argument -> [R]
        Lexical n -> L : replicate n R ++ [L]
      CApp e f -> N2 (go f) (go e)
      CLam vs e -> lam (map (go . CVar) vs) (nockToNoun (go e))
      CAtm a -> N1 (A a)
      CCel e f -> cell (go e) (go f)
      CIsC e -> N3 (go e)
      CSuc e -> N4 (go e)
      CEql e f -> N5 (go e) (go f)
      CIft e t f -> N6 (go e) (go t) (go f)
      CLet e f -> error "actually I didn't implement variable lookup sorry"
      CJet a e -> N11 (Assoc 9999 (N1 (A a))) (go e)

    lam vfs ef = NC (N1 (A 8)) (NC (NC (N1 (A 1)) vars) (N1 ef))
      where
        vars = foldr NC (N1 (A 0)) vfs

-- | The proposed new calling convention
copy :: Ord a => Exp a -> Nock
copy = copyToNock . toCopy


-- x. y. x
-- old: [8 [1 0] [1 8 [1 0] [1 0 30] 0 1] 0 1]
--      =+  0  =
