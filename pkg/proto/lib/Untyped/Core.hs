module Untyped.Core where

import ClassyPrelude

import Bound
import Control.Monad.Writer hiding (fix)
import Data.Deriving (deriveEq1, deriveOrd1, deriveRead1, deriveShow1)
import qualified Data.Function as F
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Data.Void

import Dashboard (pattern FastAtom)
import Nock
import Noun

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
  | Fix (Scope () Exp a)
  | Zap
  deriving (Functor, Foldable, Traversable)

deriveEq1   ''Exp
deriveOrd1  ''Exp
deriveRead1 ''Exp
deriveShow1 ''Exp
makeBound   ''Exp

deriving instance Eq a => Eq (Exp a)
deriving instance Ord a => Ord (Exp a)
deriving instance Read a => Read (Exp a)
deriving instance Show a => Show (Exp a)

lam :: Eq a => a -> Exp a -> Exp a
lam v e = Lam (abstract1 v e)

ledt :: Eq a => a -> Exp a -> Exp a -> Exp a
ledt v e f = Let e (abstract1 v f)

fix :: Eq a => a -> Exp a -> Exp a
fix v e = Fix (abstract1 v e)

-- | The expression that returns the given noun as a constant.
con :: Noun -> Exp a
con = \case
  A a   -> Atm a
  C n m -> Cel (con n) (con m)

data CExp a
  = CVar a
  | CSef a
  | CApp (CExp a) (CExp a)
  | CLam [a] (CExp (Var () Int))
  | CAtm Atom
  | CCel (CExp a) (CExp a)
  | CIsC (CExp a)
  | CSuc (CExp a)
  | CEql (CExp a) (CExp a)
  | CIft (CExp a) (CExp a) (CExp a)
  | CLet (CExp a) (CExp (Var () a))
  | CJet Atom (CExp a)
  | CFix [a] (CExp (Var () Int))
  | CZap
  deriving (Functor, Foldable, Traversable)

deriveEq1   ''CExp
deriveOrd1  ''CExp
deriveRead1 ''CExp
deriveShow1 ''CExp

deriving instance Eq a => Eq (CExp a)
deriving instance Ord a => Ord (CExp a)
deriving instance Read a => Read (CExp a)
deriving instance Show a => Show (CExp a)

data Manner a
  = Direct a
  | Selfish a
  deriving (Functor, Foldable, Traversable)

rude :: Manner a -> a
rude = \case
  Direct x  -> x
  Selfish x -> x

toCopy :: Ord a => Exp a -> CExp b
toCopy = fst . runWriter . go \v -> error "toCopy: free variable"
  where
    go :: Ord a => (a -> Manner c) -> Exp a -> Writer (Set a) (CExp c)
    go env = \case
      Var v     -> do
        tell (singleton v)
        case env v of
          Direct  v' -> pure (CVar v')
          Selfish v' -> pure (CSef v')
      App e f   -> CApp <$> go env e <*> go env f
      Atm a     -> pure (CAtm a)
      Cel e f   -> CCel <$> go env e <*> go env f
      IsC e     -> CIsC <$> go env e
      Suc e     -> CSuc <$> go env e
      Eql e f   -> CEql <$> go env e <*> go env f
      Ift e t f -> CIft <$> go env e <*> go env t <*> go env f
      Jet a e   -> CJet a <$> go env e
      Zap       -> pure CZap
      Let e s   -> do
        ce <- go env e
        let
          env' = \case
            B () -> Direct (B ())
            F x  -> fmap F (env x)
        cf <- retcon removeBound (go env' (fromScope s))
        pure (CLet ce cf)
      Fix s     -> lam s env CFix Selfish
      Lam s     -> lam s env CLam Direct

    lam s env ctor manner =
      writer
        ( ctor (rude . env <$> Set.toAscList usedLexicals) ce
        , usedLexicals
        )
      where
        (ce, usedVars) = runWriter $ go env' $ fromScope s
        env' = \case
          B () -> manner $ B ()
          F v  -> env v $> F (Set.findIndex v usedLexicals)
        usedLexicals = removeBound usedVars

    removeBound :: (Ord a, Ord b) => Set (Var b a) -> Set a
    removeBound = mapMaybeSet \case
      B _ -> Nothing
      F v -> Just v

-- | Like censor, except you can change the type of the log
retcon :: (w -> uu) -> Writer w a -> Writer uu a
retcon f = mapWriter \(a, m) -> (a, f m)

-- I begin to wonder why there aren't primary abstractions around filtering.
mapMaybeSet :: (Ord a, Ord b) => (a -> Maybe b) -> Set a -> Set b
mapMaybeSet f = setFromList . mapMaybe f . toList

-- Possible improvements:
--   - a "quote and unquote" framework for nock code generation (maybe)
copyToNock :: CExp a -> Nock
copyToNock = go \v -> error "copyToNock: free variable"
  where
    -- if you comment out this declaration, you get a type error!
    go :: (a -> Path) -> CExp a -> Nock
    go env = \case
      CVar v     -> N0 (toAxis $ env v)
      CSef v     -> N2 (N0 $ toAxis $ env v) (N0 $ toAxis $ env v)
      CApp e f   -> N2 (go env f) (go env e)
      CAtm a     -> N1 (A a)
      CCel e f   -> cell (go env e) (go env f)
      CIsC e     -> N3 (go env e)
      CSuc e     -> N4 (go env e)
      CEql e f   -> N5 (go env e) (go env f)
      CIft e t f -> N6 (go env e) (go env t) (go env f)
      CJet a e   -> jet a (go env e)
      CZap       -> N0 0
      CLet e f   -> N8 (go env e) (go env' f)
        where
          env' = \case
            B ()   -> [L]
            F v    -> R : env v
      CLam vs e  -> lam (map (go env . CVar) vs) (go (lamEnv vs) e)
      CFix vs e  ->
        N7
          (lam (map (go env . CVar) vs) (go (lamEnv vs) e))
          (N2 (N0 1) (N0 1))

    lamEnv vs = if null vs
      then \case
        B () -> []
        F _  -> error "copyToNock: unexpected lexical"
      else \case
        B () -> [R]
        F i  -> L : posIn i (length vs)
      
    jet a ef =
      NC
        (N1 (A 11))
        (NC
          (N1
            (C (A FastAtom)
              (C (A 1) (A a))))
          ef)
    lam vfs ef = case layOut vfs of
      Nothing -> N1 (nockToNoun ef)
      Just pr -> NC (N1 (A 8)) $ NC (NC (N1 (A 1)) pr) $ N1 (nockToNoun ef)

cell :: Nock -> Nock -> Nock
cell (N1 n) (N1 m) = N1 (C n m)
cell ef ff = NC ef ff

layOut :: [Nock] -> Maybe Nock
layOut = \case
  []  -> Nothing
  [x] -> Just x
  xs  -> Just $ NC (fromJust $ layOut l) (fromJust $ layOut r)
    where
      (l, r) = splitAt (length xs `div` 2) xs

posIn :: Int -> Int -> Path
posIn 0 1 = []
posIn i n
  | i < 0 || n <= i = error ("posIn: " <> show i <> " out of bound " <> show n)
  | i < mid         = L : posIn i mid
  | otherwise       = R : posIn (i - mid) (n - mid)
  where mid = n `div` 2

-- | The proposed new calling convention
copy :: Ord a => Exp a -> Nock
copy = copyToNock . toCopy

-- | Decrements its argument.
decrement :: Exp String
decrement = lam "a" $ App (fix "f" $ lam "b" $ Ift (Eql (Var "a") (Suc (Var "b"))) (Var "b") (App (Var "f") (Suc (Var "b")))) (Atm 0)
