{-# LANGUAGE OverloadedLists #-}

module Language.Attila.IR where

import ClassyPrelude hiding (either, fail, try)
import GHC.Natural
import Control.Lens
import Data.Vector (Vector, (!?))
import Control.Monad.Fail
import Control.Arrow ((>>>))
import Data.ChunkedZip (Zip)

--------------------------------------------------------------------------------

type Nat = Natural
type Vec = Vector

data Ty
    = Nat
    | Sum (Vec Ty)
    | Mul (Vec Ty)
    | Nok Ty Ty
    | Fix Ty
    | All Ty
    | Ref Nat
  deriving (Eq, Ord, Show)

{-
  An IR Expression

  Formulas and subject manipulation:

  - Sub -- Reference the current subject.
  - Lam -- A formula (with the type for its subject)
  - Wit -- Run an expression against a new subject.
  - Fir -- Run a formula against a subject.

  Atoms:

  - Lit -- An atom literal.
  - Inc -- Increment an atom.
  - Eke -- Atom equality.

  Product Types:

  - Tup -- Construct a product type.
  - Get -- Get a field out of a product.
  - Mod -- Update a field of a product.

  Sum Types:

  - Cho -- Construct a (branch of a) sum type.
  - Eat -- Pattern match (switch) on a sum type.
-}
data Exp
  = Sub
  | Lam Ty Exp
  | Wit Exp Exp
  | Fir Exp Exp
  | Lit Nat
  | Inc Exp
  | Eke Exp Exp
  | Tup (Vec Exp)
  | Get Exp Nat
  | Cho (Vec Ty) Nat Exp
  | Eat Exp (Vec Exp)

newtype Infer a = Infer { runInfer :: Either Text a }
  deriving newtype (Eq, Ord, Show, Functor, Applicative, Monad)

instance MonadFail Infer where
  fail = Infer . Left . pack

infGuard :: String -> Bool -> Infer ()
infGuard _   True  = pure ()
infGuard msg False = fail msg

infer :: Ty -> Exp -> Infer Ty
infer sub Sub             = pure sub
infer sub (Lam lub b)     = Nok lub <$> infer lub b
infer sub (Wit new bod)   = do newSub <- infer sub new
                               infer newSub bod
infer sub (Fir new bod)   = do newSub <- infer sub new
                               infer newSub bod
infer _   (Lit _)         = pure Nat
infer sub (Inc exp)       = do eTy <- infer sub exp
                               infGuard "bad-inc" (eTy == Nat)
                               pure Nat
infer sub (Eke ex1 ex2)   = do ty1 <- infer sub ex1
                               ty2 <- infer sub ex2
                               infGuard "bad-eq" (ty1 == Nat && ty2 == Nat)
                               pure Nat
infer sub (Tup exps)      = Mul <$> traverse (infer sub) exps
infer sub (Get tup n)     = infer sub tup >>= inferGet n
infer sub (Cho tys n exp) = infer sub exp >>= inferCho tys n
infer sub (Eat exp bods)  = inferEat sub exp bods

inferGet :: Nat -> Ty -> Infer Ty
inferGet n = \case Mul tys -> idx tys
                   _       -> fail "not-mul"
  where
    idx tys = (tys !? fromIntegral n) & \case
                Nothing -> fail "mul-bad-index"
                Just ty -> pure ty

inferCho :: Vec Ty -> Nat -> Ty -> Infer Ty
inferCho tys n ty = do
  (tys !? fromIntegral n) & \case
    Nothing -> fail "cho-bad-index"
    Just tu -> do infGuard "cho-bad-match" (tu == ty)
                  pure (Sum tys)

unify :: Vec Ty -> Infer Ty
unify = toList >>> \case []   -> pure voidTy
                         x:xs -> do infGuard "bad-unify" (all (== x) xs)
                                    pure x

zipWithM :: (Monad m, Traversable seq, Zip seq)
         => (a -> b -> m c) -> seq a -> seq b -> m (seq c)
zipWithM f xs ys = sequence (zipWith f xs ys)

inferEat :: Ty -> Exp -> Vec Exp -> Infer Ty
inferEat sub exp bods =
    infer sub exp >>= \case Sum tys -> checkSum tys
                            _       -> fail "eat-not-sum"
  where
    checkSum :: Vec Ty -> Infer Ty
    checkSum tys = do
      infGuard "eat-bad-len" (length tys == length bods)
      unify =<< zipWithM checkBranch tys bods

    checkBranch :: Ty -> Exp -> Infer Ty
    checkBranch brTy exp = infer (pair brTy sub) exp

--------------------------------------------------------------------------------

unit :: Ty
unit = Mul []

voidTy :: Ty
voidTy = Sum []

pair :: Ty -> Ty -> Ty
pair x y = Mul [x,y]

either :: Ty -> Ty -> Ty
either x y = Sum [x, y]

--------------------------------------------------------------------------------

tAtom :: Ty
tAtom = Nat

tNoun :: Ty
tNoun = Fix $ either Nat (pair (Ref 0) (Ref 0))

tOpt :: Ty
tOpt = All $ either unit (Ref 0)

tEith :: Ty
tEith = All $ All $ either (Ref 1) (Ref 0)

--------------------------------------------------------------------------------

{-
data Exp
  = Sub
  | Lam Ty Exp
  | Wit Exp Exp
  | Fir Exp Exp
  | Lit Nat
  | Inc Exp
  | Eke Exp Exp
  | Tup (Vec Exp)
  | Get Exp Nat
  | Cho (Vec Ty) Nat Exp
  | Eat Exp (Vec Exp)
-}

try :: Exp -> Either Text Ty
try = runInfer . infer voidTy

tryTup :: Either Text Ty
tryTup = try $ Get (Get (Tup [Lit 3, Tup [Lit 4, Lit 5]]) 1) 0

tryWid :: Either Text Ty
tryWid = try $ Wit (Lit 3) Sub

cho :: Exp
cho = Cho [Nat, Nat] 0 (Lit 0)

tryCho :: Either Text Ty
tryCho = try cho

tryEat :: Either Text Ty
tryEat = try $ Eat cho [Get Sub 0, Inc (Lit 0)]

-- Credits: Morgan, Ted, Benjamin
