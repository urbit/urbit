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

type FX = [(Path, Lenient Eff)]

data Eff
    = EHttpServer Server.Eff
    | EHttpClient Client.Eff
    | EAmes Ames.Eff
    | EBbye Noun
    | EBehn Noun
    | EBlit [Blit]
    | EBoat Noun
    | EClay Noun
    | ECrud Noun
    | EDirk Noun
    | EDoze (Maybe Wen)
    | EErgo Noun
    | EExit Noun
    | EFlog Noun
    | EForm Noun
    | EHill [Term]
    | EInit
    | ELogo Noun
    | EMass Noun
    | ENewt Noun
    | EOgre Noun
    | ESend [Blit]
    | ESync Noun
    | ETerm Noun
    | EThou Noun
    | ETurf (Nullable (PutDel, [Cord])) -- TODO Unsure
    | EVega Noun
    | EWest Noun
    | EWoot Noun
  deriving (Eq, Ord, Show)

data Blit
    = Bel
    | Clr
    | Hop Word64
    | Lin [Char]
    | Mor
    | Sag Path Noun
    | Sav Path Atom
    | Url Cord
  deriving (Eq, Ord, Show)

data Varience = Gold | Iron | Lead

deriveNoun ''Blit
deriveNoun ''Eff
deriveNoun ''PutDel
deriveNoun ''Varience
