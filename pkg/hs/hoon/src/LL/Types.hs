module LL.Types where

import ClassyPrelude hiding (union, intersect)
import IR.Ty
import Control.Lens
import Control.Lens.TH
import Control.Monad.Fix
import Data.Void

import Control.Category ((>>>))
import Data.Function ((&))
import Data.Maybe (fromJust)

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Prelude


-- Low-Level Representation ----------------------------------------------------

data LLDir = L | R | Ctx
  deriving (Eq, Ord, Show)

type LLPath = [LLDir]

data LL t
    = LWith t (LL t) (LL t)
    | LAxis t LLPath
    | LEdit t LLPath (LL t) (LL t)
    | LFire t Sym (Map Sym t)
    | LAtom t Nat
    | LPair t (LL t) (LL t)
    | LCore t (Map Sym (LL t))
    | LSucc t (LL t)
    | LTest t (LL t) (LL t) (LL t)
    | LCelQ t (LL t)
    | LEqlQ t (LL t) (LL t)
  deriving (Eq, Ord, Show, Functor)

type LLTy = LL Ty

llYes, llNo :: LLTy
llYes = LAtom tyBool 0
llNo  = LAtom tyBool 1

llTy :: Lens (LL a) (LL a) a a
llTy = lens get set
  where
    get = \case
      LWith t _ _     -> t
      LAxis t _       -> t
      LEdit t _ _ _   -> t
      LFire t _ _     -> t
      LAtom t _       -> t
      LPair t _ _     -> t
      LCore t _       -> t
      LSucc t _       -> t
      LTest t _ _ _   -> t
      LCelQ t _       -> t
      LEqlQ t _ _     -> t

    set ty t = ty & \case
      LWith _ x y     -> LWith t x y
      LAxis _ x       -> LAxis t x
      LEdit _ x y z   -> LEdit t x y z
      LFire _ x bat   -> LFire t x bat
      LAtom _ x       -> LAtom t x
      LPair _ x y     -> LPair t x y
      LCore _ x       -> LCore t x
      LSucc _ x       -> LSucc t x
      LTest _ x y z   -> LTest t x y z
      LCelQ _ x       -> LCelQ t x
      LEqlQ _ x y     -> LEqlQ t x y
