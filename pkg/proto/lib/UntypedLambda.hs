module UntypedLambda where

import ClassyPrelude

import Bound
import Data.Deriving (deriveEq1, deriveOrd1, deriveRead1, deriveShow1)
import Control.Monad.State

import Nock

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

newtype Ref = Ref { unRef :: Int }

nextRef :: Ref -> Ref
nextRef = Ref . (+ 1) . unRef

old :: Exp a -> Nock
old = undefined


-- x. y. x
-- old: [8 [1 0] [1 8 [1 0] [1 0 30] 0 1] 0 1]
--      =+  0  =
