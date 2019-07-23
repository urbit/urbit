module Vere.FX(FX, Eff(..), Blit(..), Varience(..), PutDel(..)) where

import UrbitPrelude hiding (Term)
import Urbit.Time
import Vere.Ovum

import qualified Vere.Ames        as Ames
import qualified Vere.Http.Server as Server
import qualified Vere.Http        as Http


--------------------------------------------------------------------------------

data PutDel = PDPut | PDDel
  deriving (Eq, Ord, Show)

type FX = [(EvilPath, Todo Eff)]

type Eff = AtomCell EffAtom EffCell

data EffAtom = EAInit | EAVoid
  deriving (Eq, Ord, Show)

data Meth = MConn | MDelt | MGet | MHead | MOpts | MPost | MPut | MTrac
  deriving (Eq, Ord, Show)

data HttpOp = HttpOp Meth (NounMap Cord [Cord]) (Maybe Octs)
  deriving (Eq, Ord, Show)

data EffCell
    = ECAmes Ames.Eff
    | ECBbye ()
    | ECBehn Void
    | ECBlit [Blit]
    | ECBoat
    | ECClay Void
    | ECCrud Void
    | ECDirk Void
    | ECDoze (Maybe Wen)
    | ECErgo Term [(Path, Maybe Mime)]
    | ECThus Atom (Maybe (PUrl, HttpOp))
    | ECExit Void
    | ECFlog Void
    | ECForm Void
    | ECHill [Term]
    | ECLogo ()
    | ECMass Void
    | ECNewt Void
    | ECOgre Void
    | ECSend Lane Bytes
    | ECSync Void
    | ECTerm Void
    | ECThou Void
    | ECTurf (Nullable (Path, Maybe Void))
    | ECVega Void
    | ECWest Void
    | ECWoot Void
    | ECSetConfig Server.Config
    | ECRequest   Word Http.Request
    | ECResponse  Http.RawEvent
  deriving (Eq, Ord, Show)

type Blit = AtomCell BlitAtom BlitCell

data BlitAtom = Bel | Clr | Mor
  deriving (Eq, Ord, Show)

data BlitCell
    = Hop Word64
    | Lin [Char]
    | Sag Path Noun
    | Sav Path Atom
    | Url Cord
  deriving (Eq, Ord, Show)

data Varience = Gold | Iron | Lead

deriveNoun ''BlitAtom
deriveNoun ''BlitCell
deriveNoun ''EffAtom
deriveNoun ''EffCell
deriveNoun ''PutDel
deriveNoun ''Varience
deriveNoun ''Meth
deriveNoun ''HttpOp
