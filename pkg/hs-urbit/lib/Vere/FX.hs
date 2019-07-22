module Vere.FX(FX, Eff(..), Blit(..), Varience(..), PutDel(..)) where

import UrbitPrelude hiding (Term)
import Urbit.Time
import Vere.Ovum

import qualified Vere.Ames        as Ames
import qualified Vere.Http.Client as Client
import qualified Vere.Http.Server as Server


--------------------------------------------------------------------------------

data PutDel = PDPut | PDDel
  deriving (Eq, Ord, Show)

type FX = [(Path, Todo Eff)]

type Eff = AtomCell EffAtom EffCell

data EffAtom = EAInit | EAVoid
  deriving (Eq, Ord, Show)

data EffCell
    = ECHttpServer Server.Eff
    | ECHttpClient Client.Eff
    | ECAmes Ames.Eff
    | ECBbye Noun
    | ECBehn Noun
    | ECBlit [Blit]
    | ECBoat Noun
    | ECClay Noun
    | ECCrud Noun
    | ECDirk Noun
    | ECDoze (Maybe Wen)
    | ECErgo Noun
    | ECExit Noun
    | ECFlog Noun
    | ECForm Noun
    | ECHill [Term]
    | ECInit
    | ECLogo Noun
    | ECMass Noun
    | ECNewt Noun
    | ECOgre Noun
    | ECSend [Blit]
    | ECSync Noun
    | ECTerm Noun
    | ECThou Noun
    | ECTurf (Nullable (PutDel, [Cord])) -- TODO Unsure
    | ECVega Noun
    | ECWest Noun
    | ECWoot Noun
    | ECSetConfig Noun
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
