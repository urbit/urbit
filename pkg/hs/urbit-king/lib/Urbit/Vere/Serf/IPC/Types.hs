{-# LANGUAGE StrictData #-}
{-# OPTIONS_GHC -ddump-splices #-}


module Urbit.Vere.Serf.IPC.Types where

import Urbit.Prelude hiding ((<|))
import Urbit.Arvo                   (Ev, FX)
import Urbit.Noun.Time              (Wen)
import Urbit.Vere.Serf.Types

-- Private data structures for Urbit.Vere.Serf.IPC, but made StrictData without
-- making the rest of Urbit.Vere.Serf.IPC strict.

data Live
  = LExit Atom -- exit status code
  | LSave EventId
  | LCram EventId
  | LPack ()
 deriving (Show)

data Play
  = PDone Mug
  | PBail PlayBail
 deriving (Show)

data Scry
  = SDone (Maybe (Term, Noun))
  | SBail Goof
 deriving (Show)

data Work
  = WDone EventId Mug FX
  | WSwap EventId Mug (Wen, Noun) FX
  | WBail [Goof]
 deriving (Show)

data Writ
  = WLive Live
  | WPeek Atom Gang ScryReq
  | WPlay EventId [Noun]
  | WWork Atom Wen Ev
 deriving (Show)

data Plea
  = PLive ()
  | PRipe SerfInfo
  | PSlog Slog
  | PFlog Cord
  | PPeek Scry
  | PPlay Play
  | PWork Work
 deriving (Show)

deriveNoun ''Live
deriveNoun ''Play
deriveNoun ''Scry
deriveNoun ''Work
deriveNoun ''Writ
deriveNoun ''Plea

