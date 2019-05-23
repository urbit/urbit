{-# LANGUAGE OverloadedLists #-}

module Language.Attila.IR where

import ClassyPrelude hiding (fail, try)
import GHC.Natural
import Control.Lens
import Data.Vector (Vector, (!), (!?))
import Control.Monad.Fail
import Control.Arrow ((>>>))
import Data.ChunkedZip (Zip)
import Language.Hoon.Nock.Types
import Text.Show.Pretty (pPrint)

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

guardInfer :: String -> Bool -> Infer ()
guardInfer _   True  = pure ()
guardInfer msg False = fail msg

unify :: Vec Ty -> Infer Ty
unify = toList >>> \case []   -> pure tVoid
                         x:xs -> do let err = "bad-unify " <> show (x:xs)
                                    guardInfer err (all (== x) xs)
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
inferGet n ty = do
  Mul tys <- pure ty
  maybe (fail "mul-bad-index") pure (tys !? fromIntegral n)

inferCho :: Vec Ty -> Nat -> Ty -> Infer Ty
inferCho tys n ty = do
  tu <- maybe (fail "cho-bad-index") pure (tys !? fromIntegral n)
  unify [tu, ty]
  pure (Sum tys)

inferEat :: Ty -> Exp -> Vec Exp -> Infer Ty
inferEat sub exp bods = do
    Sum tys <- infer sub exp
    guardInfer "eat-bad-len" (length tys == length bods)
    unify =<< zipWithM checkBranch tys bods
  where
    checkBranch :: Ty -> Exp -> Infer Ty
    checkBranch brTy exp = infer (tSum (tSum Nat brTy) sub) exp

--------------------------------------------------------------------------------

tPair :: Ty -> Ty -> Ty
tPair x y = Mul [x,y]

tSum :: Ty -> Ty -> Ty
tSum x y = Sum [x, y]

tUnit, tVoid, tAtom, tNoun, tOpt, tEith, tBool, tOrd, tTop :: Ty
tUnit = Mul []
tVoid = Sum []
tAtom = Nat
tNoun = Fix $ tSum Nat (tSum (Ref 0) (Ref 0))
tOpt  = All $ tSum tUnit (Ref 0)
tEith = All $ All $ tSum (Ref 1) (Ref 0)
tBool = tSum tUnit tUnit
tOrd  = Sum [tUnit, tUnit, tUnit]
tTop  = All $ Ref 0


-- Expression Examples ---------------------------------------------------------

tup2 :: Exp -> Exp -> Exp
tup2 x y = Tup [x, y]

choEx, tupEx, widEx, eatEx :: Exp
choEx = Cho [Nat, Nat] 0 (Lit 0)
tupEx = Get 0 $ Get 1 $ tup2 (Lit 3) $ tup2 (Lit 4) (Lit 5)
widEx = Wit (Lit 3) Sub
eatEx = Eat choEx [Get 1 (Get 0 Sub), Inc (Lit 0)]


--------------------------------------------------------------------------------

try :: Exp -> IO ()
try e = do
  putStrLn "<exp>"
  pPrint e
  putStrLn "</exp>\n"
  putStrLn "<type>"
  pPrint (runInfer (infer tTop e))
  putStrLn "</type>\n"
  putStrLn "<nock>"
  pPrint (compile tTop e)
  putStrLn "</nock>\n"

tryTup :: IO ()
tryTup = try tupEx

tryWid :: IO ()
tryWid = try widEx

tryCho :: IO ()
tryCho = try choEx

tryEat :: IO ()
tryEat = try eatEx

tryAll :: IO ()
tryAll = tryTup >> tryWid >> tryCho >> tryEat

--------------------------------------------------------------------------------

-- TODO Record layout.
compile :: Ty -> Exp -> Either Text Nock
compile sut = \case
  Sub ->
    pure (NZeroAxis 1)
  Lit n ->
    pure (NOneConst (Atom $ fromIntegral n))
  Inc x -> do
    nock <- compile sut x
    pure (NFourSucc nock)
  Cho tys n exp -> do
    nock <- compile sut exp
    pure (NCons (NOneConst (Atom (fromIntegral n))) nock)
  Get n exp -> do
    vecTy   <- runInfer (infer sut exp)
    vecNock <- compile sut exp
    axis    <- case vecTy of
                 Mul tys -> pure (getAxis n (fromIntegral $ length tys))
                 ty      -> Left ("get-not-mul: " <> tshow ty)
    pure (NSevenThen vecNock (NZeroAxis $ fromIntegral axis))

  Tup xs -> do
    nock <- genCons sut (toList xs)
    pure nock

  Eat exp brs -> do
    headTy     <- runInfer (infer sut exp)
    nock       <- compile sut exp
    newSubjTys <- headTy & \case
                    Sum tys -> pure (tys <&> (\x -> tSum (tSum Nat x) sut))
                    _       -> Left "you are dumb"

    nocks <- zipWithM compile newSubjTys brs

    pure (NEightPush nock (cases (toList nocks)))

  Eke x y -> do
    nock1 <- compile sut x
    nock2 <- compile sut y
    pure (NFiveEq nock1 nock2)

  Wit ex1 ex2 -> do
    sut'  <- runInfer (infer sut ex1)
    nock1 <- compile sut ex1
    nock2 <- compile sut' ex2
    pure (NSevenThen nock1 nock2)

  Lam ty exp -> do
    NOneConst . nockToNoun <$> compile ty exp

  Fir sub for -> do
    subNock <- compile sut sub
    forNock <- compile sut for
    pure (NTwoCompose subNock forNock)
    -- TODO Nock nine

zapZap :: Nock
zapZap = NZeroAxis 0

genCons :: Ty -> [Exp] -> Either Text Nock
genCons sut []     = compile sut (Lit 0)
genCons sut [x]    = compile sut x
genCons sut (x:xs) = do n  <- compile sut x
                        ns <- compile sut (Tup (fromList xs))
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
