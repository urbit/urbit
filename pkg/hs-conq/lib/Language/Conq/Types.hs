{-# LANGUAGE StandaloneDeriving #-}

module Language.Conq.Types where

import ClassyPrelude hiding ((<.>), Left, Right, hash, cons)
import Data.Type.Equality
import Type.Reflection
import Data.Coerce
import GHC.Natural
import Control.Category
import Data.Flat
import Data.Flat.Bits
import Data.Bits
import Data.Vector (generate)
import Data.Monoid.Unicode ((∅))
import Data.List.NonEmpty (NonEmpty(..))

import Control.Lens               ((&))
import Control.Monad.Except       (ExceptT, runExceptT)
import Control.Monad.State        (State, get, put, evalState, runState)
import Control.Monad.Trans.Except (throwE)
import Data.Bits                  ((.|.), shiftL, shiftR)
import System.IO.Unsafe           (unsafePerformIO)
import Text.Show                  (showString, showParen)

import qualified Prelude                as P
import qualified Crypto.Hash.SHA256     as SHA256
import qualified Data.ByteString.Base58 as Base58
import qualified Data.ByteString        as B
import qualified Data.ByteString.Unsafe as B


-- Utilities -------------------------------------------------------------------

type Tup a b = (a, b)

data Sum a b = L a | R b
  deriving (Eq, Ord)

-- SHA256
newtype SHA256 = SHA256 { unSHA256 :: ByteString }
  deriving newtype (Eq, Ord, Flat, Show, Hashable, NFData)


-- RTS Values ------------------------------------------------------------------

data Val
    = VV                    --  Void
    | VN                    --  Null
    | V0 !Val               --  Left
    | V1 !Val               --  Right
    | VP !Val !Val          --  Pair
    | VT !Val !Exp Val      --  Thunk
    | VR !SHA256            --  Grain
  deriving (Eq, Ord, Show, Generic, Flat)


-- Types -----------------------------------------------------------------------

data TyExp
    = TENil
    | TESum TyExp TyExp
    | TETup TyExp TyExp
    | TEFor TyExp TyExp
    | TEAll TyExp
    | TEFix TyExp
    | TERef Int
  deriving (Eq, Ord, Show, Generic, Flat)

data Ty
    = TNil
    | TSum Ty Ty
    | TTup Ty Ty
    | TFor Ty Ty
    | TVar Int
  deriving (Eq, Show, Ord)


-- Axe -- Conq Encoding --------------------------------------------------------

data Flag = T | F
  deriving (Eq, Ord, Show, Generic, Flat)

data Hint = HLazy | HMark | HType TyExp | HFast
  deriving (Eq, Ord, Show, Generic, Flat)

deriving instance Flat a => Flat (NonEmpty a)

data Axe
    = AAxis [Flag]
    | APure Val
    | AEval
    | ATong (NonEmpty Flag)
    | ACons Axe Axe
    | ACase Axe Axe
    | AWith Axe Axe
    | AHint Hint
  deriving (Eq, Ord, Show, Generic, Flat)

instance Semigroup Axe where
  AAxis xs <> AAxis ys = AAxis (ys <> xs)
  ATong xs <> ATong ys = ATong (ys <> xs)
  x <> y               = y `AWith` x

instance Monoid Axe where
  mempty = AAxis []


-- Exp -- Conq ASTs ------------------------------------------------------------

data Exp
    = ESubj
    | EPure Val
    | EEval
    | ELeft
    | EWrit
    | EHead
    | ETail
    | EDist
    | EWith Exp Exp
    | ECons Exp Exp
    | ECase Exp Exp
    | EType TyExp
    | EMark
    | ELazy
    | EFast
    | EQuot Exp
    | ETape Exp -- Unquote
  deriving (Eq, Ord, Show, Generic, Flat)

instance Semigroup Exp where
  x <> y = EWith y x

instance Monoid Exp where
  mempty = ESubj


-- Conq -- Typed-Embedding -----------------------------------------------------

data Conq s r where
  Subj :: Conq s s
  Pure :: v -> Conq s v
  Left :: Conq a (Sum a b)
  Writ :: Conq b (Sum a b)
  Head :: Conq (Tup a b) a
  Tail :: Conq (Tup a b) b
  Cons :: Conq s a -> Conq s b -> Conq s (Tup a b)
  Case :: Conq a r -> Conq b r -> Conq (Sum a b) r
  Dist :: Conq (Tup (Sum a b) s) (Sum (Tup a s) (Tup b s))
  With :: (Conq s a) -> ((Conq a r) -> (Conq s r))
  Eval :: Conq (Tup a (Conq a r)) r
  Mark :: Conq a a
  Lazy :: Conq (Tup s (Conq s a)) (Tup s (Conq s a))

instance Category Conq where
  id  = Subj
  (.) = flip With
