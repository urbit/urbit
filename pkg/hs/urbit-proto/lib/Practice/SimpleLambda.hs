module Practice.SimpleLambda where

-- import ClassyPrelude hiding (find)

-- import Numeric.Natural

-- import Nock

-- type Atom = Natural
-- type Aura = Text
-- type Name = Text

-- data Type
--   = Atom Aura
--   | Func Type Type
--   | Cell Type Type
--   deriving (Eq, Ord, Show)

-- data Term
--   = Var Name
--   -- Introduction forms
--   | Atm Atom
--   | Lam Name Type Term
--   | Con Term Term
--   -- Elimination forms
--   | Inc Term
--   | App Term Term
--   | Fst Term
--   | Snd Term
--   -- Other forms
--   | Eql Term Term
--   | Let Name Term Term
--   | Asc Term Type
--   | Fit Term Type
--   deriving (Eq, Ord, Show)

-- type Env = [(Name, Type)]

-- type Check = Either Err

-- data Err
--   = ErrFind Name
--   | ErrNest Type Type
--   | ErrFits Type Type
--   | ErrFunc Type
--   | ErrCell Type

-- instance Show Err where
-- 	show = \case
-- 	  ErrFind n -> "-find." <> unpack n
-- 	  ErrNest t u -> "nest-fail\nhave: " <> show t <> "\nneed: " <> show u
-- 	  ErrFits t u -> "fits-fail\nhave: " <> show t <> "\nneed: " <> show u
-- 	  ErrFunc t -> "need-func: " <> show t
-- 	  ErrCell t -> "need-cell: " <> show t

-- -- | Verifies that every value of the first type is also a value of the second.
-- nest :: Type -> Type -> Check ()
-- nest (Atom a)     (Atom b)     = if isPrefixOf b a  -- Note: only one way
-- 	                                 then pure ()
-- 	                                 else Left $ ErrNest (Atom a) (Atom b)
-- nest (Func a1 r1) (Func a2 r2) = do nest a2 a1; nest r1 r2
-- nest (Cell h1 t1) (Cell h2 t2) = do nest h1 h2; nest t1 t2
-- nest t@Atom{} u = Left $ ErrNest t u
-- nest t@Func{} u = Left $ ErrNest t u
-- nest t@Cell{} u = Left $ ErrNest t u

-- -- | Verifies that the first type is *coercible* to the second.
-- fits :: Type -> Type -> Check ()
-- fits (Atom a)     (Atom b)     = if isPrefixOf a b || isPrefixOf b a
-- 	                                 then pure ()
-- 	                                 else Left $ ErrNest (Atom a) (Atom b)
-- fits (Func a1 r1) (Func a2 r2) = do fits a2 a1; fits r1 r2
-- fits (Cell h1 t1) (Cell h2 t2) = do fits h1 h2; fits t1 t2
-- fits t@Atom{} u = Left $ ErrNest t u
-- fits t@Func{} u = Left $ ErrNest t u
-- fits t@Cell{} u = Left $ ErrNest t u

-- find :: Env -> Name -> Check Type
-- find [] n = Left $ ErrFind n
-- find ((n,t):e) m
--   | n == m    = pure t
--   | otherwise = find e m

-- -- | Given an environment, check that a term has the given type.
-- work :: Env -> Term -> Type -> Check () 
-- work env tm ty = do
-- 	ty' <- play env tm
-- 	nest ty' ty

-- needFunc :: Env -> Term -> Check (Type, Type)
-- needFunc env tm = play env tm >>= \case
-- 	Func a b -> pure (a, b)
-- 	t -> Left $ ErrFunc t

-- needCell :: Env -> Term -> Check (Type, Type)
-- needCell env tm = play env tm >>= \case
-- 	Cell a b -> pure (a, b)
-- 	t -> Left $ ErrCell t

-- -- | Given an environment, infer the type of a term.
-- play :: Env -> Term -> Check Type
-- play env = \case
--   Var v -> find env v
--   --
--   Atm a -> pure $ Atom ""
--   Lam v ty tm -> Func ty <$> play ((v,ty):env) tm
--   Con t u -> Cell <$> play env t <*> play env u
--   --
--   Inc t -> do
--   	work env t (Atom "")
--   	pure (Atom "")
--   App f a -> do
--   	(par, res) <- needFunc env f
--   	arg <- play env a
--   	nest arg par
--   	pure res
--   Fst t -> do
--   	(hd, _) <- needCell env t
--   	pure hd
--   Snd t -> do
--   	(_, tl) <- needCell env t
--   	pure tl
--   Eql t u -> do
--   	a <- play env t
--   	b <- play env u
--   	nest a b <|> nest b a
--   	pure $ Atom "b"
--   Let v rhs bod -> do
--   	t <- play env rhs
--   	play ((v,t):env) bod
--   Asc tm ty -> work env tm ty >> pure ty
--   Fit tm ty -> play env tm >>= fits ty >> pure ty
