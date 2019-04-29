-- TODO Handle comments

module AST.Types where

import ClassyPrelude
import Data.List.NonEmpty (NonEmpty)

-- AST Types -------------------------------------------------------------------

type Nat  = Int
type Sym  = String
type Wing = [Either Nat Sym]

data Base = BVoid | BNull | BFlag | BNoun | BCell | BAtom
  deriving (Eq, Ord, Show)

data Spec
    = SBase Base               --  ^, ?
    | SFaceOp Sym Spec         --  x=@
    | SBucCol (NonEmpty Spec)  --  $:
    | SBucHep Spec Spec        --  $-, function core
    | SBucTis Sym Spec         --  $=, name
    | SBucWut (NonEmpty Spec)  --  $?, full pick
    | SBucPat Spec Spec        --  $@, atom pick
    | SBucKet Spec Spec        --  $^, cons pick
    | SBucCen (NonEmpty Spec)  --  $%, head pick
    | STuple (NonEmpty Spec)   --  [@ @]
  deriving (Eq, Ord, Show)

data Hoon
    = WutCol Hoon Hoon Hoon       --  ?:(c t f)
    | WutTis Spec Hoon            --  ?=(@ 0)
    | WutPat Hoon Hoon Hoon       --  ?@(c t f)
    | WutKet Hoon Hoon Hoon       --  ?^(c t f)
    | KetTis Sym Hoon             --  ^=(x 3)
    | ColHep Hoon Hoon            --  :-(a b)
    | ColLus Hoon Hoon Hoon       --  :+(a b c)
    | ColKet Hoon Hoon Hoon Hoon  --  :^(a b c d)
    | ColTar [Hoon]               --  :*(a as ...)
    | ColSig [Hoon]               --  :~(a as ...)
    | KetHep Spec Hoon            --  ^-(s h)
    | TisGal Hoon Hoon            --  =<(a b)
    | TisGar Hoon Hoon            --  =>(a b)
    | BarTis Hoon Hoon            --  |=(s h)
    | BarHep Hoon                 --  |-(a)
    | TisDot Wing Hoon Hoon       --  =.(a 3 a)
    | BarCen (Map Sym Hoon)       --  |%  ++  a  3  --
    | ColOp Hoon Hoon             --  [+ -]:[3 4]
    | Tupl [Hoon]                 --  [a b]
    | FaceOp Sym Hoon             --  x=y
    | Wing Wing                   --  ., a, a.b
    | Atom Nat                    --  3
    | Cord Text                   --  'cord'
    | Tape Text                   --  "tape"
    | Incr Hoon                   --  .+(3)
    | IncrIrr Hoon                --  +(3)
    | IsEq Hoon Hoon              --  .=(3 4)
    | IsEqIrr Hoon Hoon           --  =(3 4)
    | Lus                         --  +
    | Hep                         --  -
    | Pam                         --  &
    | Bar                         --  |
    | Yes                         --  %.y
    | No                          --  %.n
    | Sig                         --  ~
  deriving (Eq, Ord, Show)
