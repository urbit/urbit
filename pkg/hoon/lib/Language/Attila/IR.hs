{-# LANGUAGE OverloadedLists #-}

module Language.Attila.IR where

import ClassyPrelude hiding (either, fail, try)
import GHC.Natural
import Control.Lens
import Data.Vector (Vector, (!), (!?))
import Control.Monad.Fail
import Control.Arrow ((>>>))
import Data.ChunkedZip (Zip)
import Language.Hoon.Nock.Types

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
  | Get Nat Exp
  | Cho (Vec Ty) Nat Exp
  | Eat Exp (Vec Exp)
  deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------

zipWithM :: (Monad m, Traversable seq, Zip seq)
         => (a -> b -> m c) -> seq a -> seq b -> m (seq c)
zipWithM f xs ys = sequence (zipWith f xs ys)

--------------------------------------------------------------------------------

newtype Infer a = Infer { runInfer :: Either Text a }
  deriving newtype (Eq, Ord, Show, Functor, Applicative, Monad)

instance MonadFail Infer where
  fail = Infer . Left . pack

infGuard :: String -> Bool -> Infer ()
infGuard _   True  = pure ()
infGuard msg False = fail msg

unify :: Vec Ty -> Infer Ty
unify = toList >>> \case []   -> pure voidTy
                         x:xs -> do let err = "bad-unify " <> show (x:xs)
                                    infGuard err (all (== x) xs)
                                    pure x

--------------------------------------------------------------------------------

infer :: Ty -> Exp -> Infer Ty
infer sub Sub             = pure sub
infer sub (Lam lub b)     = Nok lub <$> infer lub b
infer sub (Wit new bod)   = do newSub <- infer sub new
                               infer newSub bod
infer sub (Fir new bod)   = do newSub <- infer sub new
                               infer newSub bod
infer _   (Lit _)         = pure Nat
infer sub (Inc exp)       = do eTy <- infer sub exp
                               unify [eTy, Nat]
infer sub (Eke ex1 ex2)   = do ty1 <- infer sub ex1
                               ty2 <- infer sub ex2
                               unify [Nat, ty1, ty2]
infer sub (Tup exps)      = Mul <$> traverse (infer sub) exps
infer sub (Get n tup)     = infer sub tup >>= inferGet n
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
    Just tu -> do unify [tu, ty]
                  pure (Sum tys)

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
    checkBranch brTy exp = infer (pair (pair Nat brTy) sub) exp

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

tBool :: Ty
tBool = either unit unit

tOrd :: Ty
tOrd = Sum [unit, unit, unit]


-- Expression Examples ---------------------------------------------------------

tup2 :: Exp -> Exp -> Exp
tup2 x y = Tup [x, y]

choEx :: Exp
choEx = Cho [Nat, Nat] 0 (Lit 0)

tupEx :: Exp
tupEx = Get 0 $ Get 1 $ tup2 (Lit 3) $ tup2 (Lit 4) (Lit 5)

widEx :: Exp
widEx = Wit (Lit 3) Sub

eatEx :: Exp
eatEx = Eat choEx [Get 1 (Get 0 Sub), Inc (Lit 0)]


--------------------------------------------------------------------------------

try :: Exp -> Either Text Ty
try = runInfer . infer voidTy

build :: Exp -> Either Text (Ty, Nock)
build = compile voidTy

tryTup :: Either Text Ty
tryTup = try tupEx

tryWid :: Either Text Ty
tryWid = try widEx

tryCho :: Either Text Ty
tryCho = try choEx

tryEat :: Either Text Ty
tryEat = try eatEx

buildTup :: Either Text (Ty, Nock)
buildTup = build tupEx

buildWid :: Either Text (Ty, Nock)
buildWid = build widEx

buildCho :: Either Text (Ty, Nock)
buildCho = build choEx

buildEat :: Either Text (Ty, Nock)
buildEat = build eatEx

--------------------------------------------------------------------------------

-- TODO Record layout.
compile :: Ty -> Exp -> Either Text (Ty, Nock)
compile sut = \case
  Sub ->
    pure (sut, NZeroAxis 1)
  Lit n ->
    pure (Nat, NOneConst (Atom $ fromIntegral n))
  Inc x -> do
    (_, nock) <- compile sut x
    pure (Nat, NFourSucc nock)
  Cho tys n exp -> do
    (_, nock) <- compile sut exp
    let tag = NOneConst (Atom (fromIntegral n))
    pure (Sum tys, NCons tag nock)
  Get n exp -> do
    (vecTy, vecNock) <- compile sut exp

    tys <- case vecTy of
             Mul tys -> pure tys
             ty      -> Left ("get-not-mul: " <> tshow ty)

    let axis = getAxis n (fromIntegral $ length tys)
    let resTy = tys ! fromIntegral n
    pure (resTy, NSevenThen vecNock (NZeroAxis $ fromIntegral axis))

  Tup xs -> do
    ty <- runInfer (infer sut (Tup xs))
    nock <- genCons sut (toList xs)
    pure (ty, nock)

  Eat exp brs -> do
    (headTy, nock) <- compile sut exp

    newSubjTys <- headTy & \case
      Sum tys -> pure $ fmap (\x -> pair (pair Nat x) sut) tys
      _       -> Left "you are dumb"

    nocks <- fmap snd <$> zipWithM compile newSubjTys brs

    resTy <- runInfer (infer sut (Eat exp brs))

    pure (resTy, NEightPush nock (cases (toList nocks)))

  Eke x y -> do
    (_, nock1) <- compile sut x
    (_, nock2) <- compile sut y
    pure (tBool, NFiveEq nock1 nock2)

  Wit ex1 ex2 -> do
    (sut',  nock1) <- compile sut ex1
    (resTy, nock2) <- compile sut' ex2
    pure (resTy, NSevenThen nock1 nock2)

  Lam ty exp -> do
    resTy <- runInfer (infer ty exp)
    nock <- (NOneConst . nockToNoun . snd) <$> compile ty exp
    pure (Nok sut resTy, nock)

  Fir sub for -> do
    (subTy, subNock) <- compile sut sub
    (forTy, forNock) <- compile sut for
    resTy <- case forTy of
               Nok _ resTy -> pure resTy
               _           -> Left "bad-fir-e"
    pure (resTy, NTwoCompose subNock forNock)
    -- TODO Nock nine

zapZap :: Nock
zapZap = NZeroAxis 0

genCons :: Ty -> [Exp] -> Either Text Nock
genCons sut []     = snd <$> compile sut (Lit 0)
genCons sut [x]    = snd <$> compile sut x
genCons sut (x:xs) = do
  (_, n)  <- compile sut x
  (_, ns) <- compile sut (Tup (fromList xs))
  pure (NCons n ns)

cases :: [Nock] -> Nock
cases = go 0
  where
    go tag []           = zapZap
    go tag (nock:nocks) = NSixIf (NFiveEq (NOneConst (Atom (fromIntegral tag)))
                                          (NZeroAxis 4))
                                 nock
                                 (go (tag+1) nocks)

getAxis :: Nat -> Nat -> Nat
getAxis 0 1                = 1
getAxis 0 len              = 2
getAxis i len | i+1 == len = 1 + getAxis (i-1) len
getAxis i len              = 2 * (1 + getAxis (i-1) len)

-- Credits: Morgan, Ted, Benjamin
