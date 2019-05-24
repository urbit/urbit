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
import Text.Show.Pretty (ppShow)

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
  | Wit Ty Exp Exp
  | Fir Exp Exp
  | Lit Nat
  | Inc Exp
  | Eke Exp Exp
  | Tup (Vec Exp)
  | Get Nat (Vec Ty) Exp
  | Cho (Vec Ty) Nat Exp
  | Eat (Vec Ty) Exp (Vec Exp)
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
infer sut Sub                = pure sut
infer sut (Lam lub b)        = Nok lub <$> infer lub b
infer sut (Wit ty new bod)   = do newSub <- infer sut new
                                  unify [ty, newSub]
                                  infer ty bod
infer sut (Fir new bod)      = do newSub <- infer sut new
                                  infer newSub bod
infer _   (Lit _)            = pure Nat
infer sut (Inc exp)          = do eTy <- infer sut exp
                                  unify [eTy, Nat]
infer sut (Eke ex1 ex2)      = do ty1 <- infer sut ex1
                                  ty2 <- infer sut ex2
                                  unify [Nat, ty1, ty2]
infer sut (Tup exps)         = Mul <$> traverse (infer sut) exps
infer sut (Get n tys tup)    = do tupTy <- infer sut tup
                                  unify [Mul tys, tupTy]
                                  maybe (fail "mul-bad-index") pure
                                    (tys !? fromIntegral n)
infer sut (Cho tys n exp)    = do ty <- infer sut exp
                                  tu <- maybe (fail "cho-bad-index") pure
                                          (tys !? fromIntegral n)
                                  unify [tu, ty]
                                  pure (Sum tys)
infer sut (Eat tys exp bods) = do expTy <- infer sut exp
                                  unify [expTy, Sum tys]
                                  guardInfer "eat-bad-len"
                                    (length tys == length bods)
                                  let checkBranch br exp =
                                        infer (tPair (tPair Nat br) sut) exp
                                  unify =<< zipWithM checkBranch tys bods

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
widEx = Wit Nat (Lit 3) Sub
tupEx = Get 0 [Nat, Nat]
      $ Get 1 [Nat, Mul [Nat, Nat]]
      $ tup2 (Lit 3) (tup2 (Lit 4) (Lit 5))

eatEx = Eat [Nat, Nat]
          choEx
          [ Get 1 [Nat, Nat]
              (Get 0 [Mul [Nat, Nat], tTop] Sub)
          , Inc (Lit 0)
          ]


--------------------------------------------------------------------------------

indent :: String -> String
indent = unlines . fmap ("        " <>) . lines

try :: Text -> Exp -> IO ()
try m e = do
  putStrLn (m <> ":")
  putStrLn "    exp:"
  putStr (pack $ indent (ppShow e))
  putStrLn "    type:"
  putStr (pack $ indent (ppShow (runInfer (infer tTop e))))
  putStrLn "    nock:"
  putStr (pack $ indent (ppShow (compile tTop e)))

tryTup :: IO ()
tryTup = try "tup" tupEx

tryWit :: IO ()
tryWit = try "wid" widEx

tryCho :: IO ()
tryCho = try "cho" choEx

tryEat :: IO ()
tryEat = try "eat" eatEx

tryAll :: IO ()
tryAll = tryTup >> tryWit >> tryCho >> tryEat

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
  Get n tys exp -> do
    vecNock <- compile sut exp
    axis    <- pure (getAxis n (fromIntegral $ length tys))
    pure (NSevenThen vecNock (NZeroAxis $ fromIntegral axis))

  Tup xs -> do
    nock <- genCons sut (toList xs)
    pure nock

  Eat tys exp brs -> do
    nock       <- compile sut exp
    newSubjTys <- pure (tys <&> (\x -> tPair (tPair Nat x) sut))
    nocks      <- zipWithM compile newSubjTys brs
    pure (NEightPush nock (cases (toList nocks)))

  Eke x y -> do
    nock1 <- compile sut x
    nock2 <- compile sut y
    pure (NFiveEq nock1 nock2)

  Wit sut' ex1 ex2 -> do
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
