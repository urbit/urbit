module Language.Hoon.Types where

import Prelude

import Language.Hoon.Nock.Types

import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.Map           (Map)
import Data.Set           (Set)
import Data.List.NonEmpty (NonEmpty)

--------------------------------------------------------------------------------

hoonVersion :: Atom
hoonVersion = 141

type Name = String  -- "term"
type Aura = Name
type Tape = String
type AtomUD = Atom

nameToAtom = error "TODO nameToAtom"

data Type = Void
          | Noun
          | Atomic Name (Maybe Atom)
          | Cell Type Type
          | Core Type  -- TODO
          | Face Name Type  -- TODO name or "tune"
          | Fork (Set Type)
          -- TODO %hint
          | Hold Type Hoon

data Polarity = Wet | Dry
  deriving (Eq, Ord, Read, Show)

data Variance = Invariant      -- "gold"
              | Contravariant  -- "iron"
              | Bivariant      -- "lead"
              | Covariant      -- "zinc"
  deriving (Eq, Ord, Read, Show)

type Claims = Map Name Spec

data Base
    = SVoid                         -- Empty Set
    | SNull                         -- ~
    | SFlag                         -- Bool
    | SNoun                         -- *
    | SCell                         -- ^
    | SAtom Aura

data Spec
    = SBase Base
    | SDebug Spot Spec
    | SLeaf Name Atom
    | Like (NonEmpty Wing)
    | Loop Name
    | Made (NonEmpty Name) Spec
    | Make Hoon [Spec]
    | Name Name Spec
    | Over Wing Spec
    --
    | BucGar Spec Spec       -- ^ $> filter: require
    | BucBuc Spec Claims     -- ^ $$ recursion
    | BucBar Spec Hoon       -- ^ $$ recursion
    | BucCab Hoon            -- $_
    | BucCol (NonEmpty Spec) -- $:
    | BucCen (NonEmpty Spec) --  $%, head pick
    | BucDot Spec Claims     --  $., read-write core
    | BucLed Spec Spec       --  $<, filter: exclude
    | BucHep Spec Spec       --  $-, function core
    | BucKet Spec Spec       --  $^, cons pick
    | BucLus Stud Spec       --  $+, standard
    | BucFas Spec Claims     --  $/, write-only core
    | BucMic Hoon            --  $;, manual
    | BucPad Spec Hoon       --  $&, repair
    | BucSig Hoon Spec       --  $~, default
    | BucTic Spec Claims     --  $`, read-only core
    | BucTis Skin Spec       --  $=, name
    | BucPat Spec Spec       --  $@, atom pick
    | BucWut (NonEmpty Spec) --  $?, full pick
    | BucZap Spec Claims     --  $!, opaque core

type Bindings hoon = Map Name (Map Name (BindingHelp, hoon))
data BindingHelp  -- "what"
data Tome

data Hoon
    = HAutocons [Hoon]
    | H_ Axis     -- shorthand which should have been a function
    | HBase Base  -- base type mold
    | Bust Base   -- base type bunt
    | HDebug Spot Hoon
    | Error Tape
    | Hand Type Nock
    | Note Note Hoon
    | Fits Hoon Wing
    | Knit [Woof]
    | HLeaf Name Atom
    | Limb Name
    | Lost Hoon
    | Rock Name Noun
    | Sand Name Noun
    | Tell (NonEmpty Hoon)
    | Tune (Either Name Tune)
    | Wing Wing
    | Yell (NonEmpty Hoon)
    | Xray ManxHoot
    -- Cores
    | BarCab Spec Alas (Bindings Hoon)
    | BarCol Hoon Hoon
    | BarCen (Maybe Name) (Bindings Hoon)
    | BarDot Hoon
    | BarKet Hoon (Bindings Hoon)
    | BarHep Hoon
    | BarSig Spec Hoon
    | BarTar Spec Hoon
    | BarTis Spec Hoon
    | BarPat (Maybe Name) (Bindings Hoon)
    | BarWut Hoon
    -- Tuples
    | ColCab Hoon Hoon
    | ColKet Hoon Hoon Hoon Hoon
    | ColHep Hoon Hoon
    | ColLus Hoon Hoon Hoon
    | ColSig [Hoon]
    | ColTar (NonEmpty Hoon)  -- was ordinary list
    -- Invocations
    | CenCab Wing [(Wing, Hoon)]
    | CenDot Hoon Hoon
    | CenHep Hoon Hoon
    | CenCol Hoon [Hoon]
    | CenTar Wing Hoon [(Wing, Hoon)]
    | CenKet Hoon Hoon Hoon Hoon
    | CenLus Hoon Hoon Hoon
    | CenSig Wing Hoon [Hoon]
    | CenTis Wing [(Wing, Hoon)]
    -- Nock
    | DotKet Spec Hoon
    | DotLus Hoon
    | DotTar Hoon Hoon
    | DotTis Hoon Hoon
    | DotWut Hoon
    -- Type conversions
    | KetBar Hoon
    | KetCen Hoon
    | KetDot Hoon Hoon
    | KetLus Hoon Hoon
    | KetHep Spec Hoon
    | KetPam Hoon
    | KetSig Hoon
    | KetTis Skin Hoon
    | KetWut Hoon
    | KetTar Spec
    | KetCol Spec
    -- Hints
    | SigBar Hoon Hoon
    | SigCab Hoon Hoon
    | SigCen Chum Hoon Tyre Hoon
    | SigFas Chum Hoon
    | SigLed Name (Maybe Hoon) Hoon
    | SigGar Name (Maybe Hoon) Hoon
    | SigBuc Name Hoon
    | SigLus Atom Hoon
    | SigPam AtomUD Hoon Hoon
    | SigTis Hoon Hoon
    | SigWut AtomUD Hoon Hoon Hoon
    | SigZap Hoon Hoon
    -- Miscellaneous
    | MicTis MarlHoot
    | MicCol Hoon [Hoon]
    | MicNet Hoon
    | MicSig Hoon [Hoon]
    | MicMic Hoon Hoon
    -- Compositions
    | TisBar Spec Hoon
    | TisCol [(Wing, Hoon)] Hoon
    | TisFas Skin Hoon Hoon
    | TisMic Skin Hoon Hoon
    | TisDot Wing Hoon Hoon
    | TisWut Wing Hoon Hoon Hoon
    | TisLed Hoon Hoon
    | TisHep Hoon Hoon
    | TisGar Hoon Hoon
    | TisKet Skin Wing Hoon Hoon
    | TisLus Hoon Hoon
    | TisSig [Hoon]
    | TisTar Name (Maybe Spec) Hoon Hoon
    | TisCom Hoon Hoon
    -- Conditionals
    | WutBar [Hoon]
    | WutHep Wing [(Spec,Hoon)]
    | WutCol Hoon Hoon Hoon
    | WutDot Hoon Hoon Hoon
    | WutKet Wing Hoon Hoon
    | WutLed Hoon Hoon
    | WutGar Hoon Hoon
    | WutLus Wing Hoon [(Spec, Hoon)]
    | WutPam [Hoon]
    | WutPat Wing Hoon Hoon
    | WutSig Wing Hoon Hoon
    | WutHax Skin Wing
    | WutTis Spec Wing
    | WutZap Hoon
    -- Special
    | ZapCom Hoon Hoon
    | ZapGar Hoon
    | ZapMic Hoon Hoon
    | ZapTis Hoon
    | ZapPat [Wing] Hoon Hoon
    | ZapWut (Either Atom (Atom, Atom)) Hoon
    | ZapZap

type Wing = [Limb]
data Limb
    = NameLimb Name
    | AxisLimb Axis  -- ^ %& tag
    | ByNameLimb Atom (Maybe Name) -- ^ ??? %| tag

data Tune
    = Tone
        { aliases :: (Map Name (Maybe Hoon))
        , bridges :: [Hoon]
        }

-- TODO
data Alas
data Spot
data Woof
data ManxHoot
data MarlHoot
data Note
data Chum
data Tyre
data Stud
data Skin

-- | Basic hoon: totally desugared
data BHoon
    = BAutocons [BHoon]
    | BDebug Spot BHoon
    | BHand Type Nock
    | BNote Note BHoon
    | BFits BHoon Wing
    | BSand Name Noun
    | BRock Name Noun
    | BTune (Either Name Tune)
    | BLost BHoon
    --
    | BBarCen (Maybe Name) (Bindings BHoon)
    | BBarPat (Maybe Name) (Bindings BHoon)
    --
    | BCenTis Wing [(Wing, BHoon)]
    --
    | BDotKet Spec BHoon
    | BDotLus BHoon
    | BDotTar BHoon BHoon
    | BDotTis BHoon BHoon
    | BDotWut BHoon
    --
    | BKetBar BHoon
    | BKetCen BHoon
    | BKetLus BHoon BHoon
    | BKetPam BHoon
    | BKetSig BHoon
    | BKetWut BHoon
    --
    | BSigGar Name (Maybe BHoon) BHoon
    | BSigZap BHoon BHoon
    --
    | BTisGar BHoon BHoon
    | BTisCom BHoon BHoon
    --
    | BWutCol BHoon BHoon BHoon
    | BWutHax Skin Wing
    --
    | BZapCom BHoon BHoon
    | BZapMic BHoon BHoon
    | BZapTis BHoon
    | BZapPat [Wing] BHoon BHoon
    | BZapZap

