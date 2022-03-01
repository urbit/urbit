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

-- | Mode for fit-checking in `fits`: nest, cast, or exact equality.
data Fit
  = FitSame  -- ^ perform a type (or value) equivalence check
  | FitNest  -- ^ perform a subtyping check
  | FitCast  -- ^ perform a coercibility check; i.e. ignore auras
  deriving (Eq, Ord, Generic)

instance Show Fit where
  show = \case
    FitCast -> "cast"
    FitNest -> "nest"
    FitSame -> "same"


-- Axial operations ------------------------------------------------------------

data Step = L | R
  deriving (Eq, Ord, Show)

hop :: Step -> Axis -> Axis
hop L = peg 2
hop R = peg 3

peg :: Axis -> Axis -> Axis
peg a = \case
  0 -> error "zero axis"  -- I guess? the hoon diverges
  1 -> a
  2 -> a * 2
  3 -> a * 2 + 1
  b -> b `mod` 2 + peg a (b `div` 2) * 2

-- | Combo of cap and mas. FIXME name change.
cut :: Axis -> Maybe (Step, Axis)
cut = \case
  0 -> error "zero axis"
  1 -> Nothing
  2 -> Just (L, 1)
  3 -> Just (R, 1)
  a -> let Just (s, b) = cut (a `div` 2)
       in Just (s, a `mod` 2 + b * 2)

run :: Axis -> [Step]
run = map fst . pop

-- | Really very sorry
pop :: Axis -> [(Step, Axis)]
pop a = case cut a of
  Nothing -> []
  Just (s, a') -> (s, a) : pop a'
