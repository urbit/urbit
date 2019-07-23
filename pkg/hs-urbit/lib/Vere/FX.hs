{-# OPTIONS_GHC -Wwarn #-}

module Vere.FX
  ( FX, Effect(..), Eff(..)
  , Blit(..), Varience(..), PutDel(..)
  , EffRoute(..)
  ) where

import UrbitPrelude hiding (Term)
import Urbit.Time
import Vere.Ovum

import qualified Vere.Http.Server as Server
import qualified Vere.Http        as Http


-- PutDel ----------------------------------------------------------------------

data PutDel = PDPut | PDDel
  deriving (Eq, Ord, Show)

deriveNoun ''PutDel

--------------------------------------------------------------------------------

data Meth = MConn | MDelt | MGet | MHead | MOpts | MPost | MPut | MTrac
  deriving (Eq, Ord, Show)

data HttpOp = HttpOp Meth (NounMap Cord [Cord]) (Maybe Octs)
  deriving (Eq, Ord, Show)

deriveNoun ''HttpOp
deriveNoun ''Meth

-- Blit ------------------------------------------------------------------------

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

deriveNoun ''Blit


-- Big Effect Sum --------------------------------------------------------------

data Eff
    = EffInit
    | EffDoze      (Maybe Wen)
    | EffVoid      Void
    | EffSend      Lane Bytes
    | EffBlit      [Blit]
    | EffRequest   Word Http.Request
    | EffHill      [Term]
    | EffSetConfig Server.Config
    | EffResponse  Http.RawEvent
    | EffTurf (Nullable (Path, Maybe Void))

    -- Verified from source code

    | EffWest Ship Path Noun
    | EffBbye ()
    | EffErgo Term [(Path, Maybe Mime)]
    | EffThus Atom (Maybe (PUrl, HttpOp))
    | EffLogo ()
    | EffOgre Term
    | EffVega ()
    | EffWoot Ship (Maybe (Maybe (Term, [Tank])))
    | EffExit ()
    | EffDirk Term  --  sync (mark mount dirty)

    -- Seems like bullshit.

    | EffMass Noun  -- Not relevant anymore
    | EffCrud Noun  -- This seems to exist, but I can't figure out the shape.
  deriving (Eq, Ord, Show)

deriveNoun ''Eff


-- Effect Route ----------------------------------------------------------------

data EffRoute
    = ERAmes
    | ERBehn
    | ERBoat
    | ERClay
    | ERHttpClient
    | ERHttpServer
    | ERInit
    | ERNewt
    | ERSync
    | ERTerm

-- I saw this somewhere, but I don't remember where. ---------------------------

data Varience = Gold | Iron | Lead

deriveNoun ''Varience


-- Top-Level Effect Type -------------------------------------------------------

type FX = [Effect]

data Effect = Effect EvilPath Eff
  deriving (Eq, Ord, Show)

deriveNoun ''Effect
