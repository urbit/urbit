module Practice.HoonCommon where

import ClassyPrelude

import Data.Char
import Numeric.Natural

import Urbit.Atom (atomUtf8)

type Atom = Natural

type Aura = Term

type Axis = Atom

-- | Base type
data Bass
  = Non
  | Cel
  | Flg
  | Nul
  | Vod
  | Fok [Atom] Aura
  | Aur Aura
  | Typ
  deriving (Eq, Ord, Show)

-- | A @tas. Frankly, would prefer to rename this `name`.
type Term = Text

-- | Whether or not an atom should be taken to be of singleton type.
data Grit
  = Rock  -- ^ should be taken to be of singleton type
  | Sand  -- ^ should be taken to be of broader type
  deriving (Eq, Ord, Read, Show, Generic)

type Wing = [Limb]

data Limb
  = Axis Axis
  | Ally Term
  deriving (Eq, Ord, Read, Show, Generic)

type Nat = Natural

printLimb :: Limb -> Text
printLimb = \case
  Axis a -> "+" <> tshow a
  Ally "" -> "%"
  Ally n -> n

printWing :: Wing -> Text
printWing = \case
  [] -> "."
  [l] -> printLimb l
  l:ls -> printLimb l <> "." <> printWing ls

heuAura :: Atom -> Aura
heuAura a = case a of
  0 -> ""
  _ -> case atomUtf8 a of
    Right t | length t > 1 && all isPrint t -> "t"
    _ -> ""
