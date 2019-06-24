{-# LANGUAGE StandaloneDeriving #-}

module Language.Conq.ForVal where

import Language.Conq.Types

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
