module Urbit.Vere.Serf.Types where

import Urbit.Prelude

import Urbit.Arvo      (Desk, Ev, FX)
import Urbit.Noun.Time (Wen)


-- Types -----------------------------------------------------------------------

type EventId = Word64

type PlayBail = (EventId, Mug, Goof)

type Slog = (Atom, Tank)

data SerfState = SerfState
  { ssLast :: !EventId
  , ssHash :: !Mug
  }
 deriving (Show, Eq)

data RipeInfo = RipeInfo
  { riProt :: !Atom
  , riHoon :: !Atom
  , riNock :: !Atom
  }
 deriving (Show)

data SerfInfo = SerfInfo
  { siRipe :: !RipeInfo
  , siStat :: !SerfState
  }
 deriving (Show)

data Fact = Fact
  { factEve :: EventId
  , factMug :: Mug
  , factWen :: Wen
  , factNon :: Noun
  }

data Flag
  = DebugRam
  | DebugCpu
  | CheckCorrupt
  | CheckFatal
  | Verbose
  | DryRun
  | Quiet
  | Hashless
  | Trace
 deriving (Eq, Ord, Show, Enum, Bounded)

data Config = Config
  { scSerf :: FilePath       --  Where is the urbit-worker executable?
  , scPier :: FilePath       --  Where is the pier directory?
  , scFlag :: [Flag]         --  Serf execution flags.
  , scSlog :: Slog -> IO ()  --  What to do with slogs?
  , scStdr :: Text -> IO ()  --  What to do with lines from stderr?
  , scDead :: IO ()          --  What to do when the serf process goes down?
  }


-- Serf Commands ---------------------------------------------------------------

type Gang = Maybe (HoonSet Ship)

type Goof = (Term, [Tank])

data EvErr = EvErr Ev (WorkError -> IO ())

{-|
  Two types of serf failures.

  - `RunSwap`: Event processing failed, but the serf replaced it with
    another event which succeeded.

  - `RunBail`: Event processing failed and all attempt to replace it
    with a failure-notice event also caused crashes. We are really fucked.
-}
data WorkError -- TODO Rename type and constructors
  = RunSwap EventId Mug Wen Noun FX -- TODO Maybe provide less info here?
  | RunBail [Goof]
  | RunOkay EventId FX

{-
  - RRWork: Ask the serf to do work, will output (Fact, FX) if work
    succeeded and call callback on failure.
  - RRSave: Wait for the serf to finish all pending work
-}
data RunReq
  = RRWork EvErr
  | RRSave ()
  | RRKill ()
  | RRPack ()
  | RRScry Gang ScryReq (Maybe (Term, Noun) -> IO ())

type ScryReq = (Each Path Demi)

data Demi
  = DemiOnce Term Desk Path
  | DemiBeam Term Beam
  deriving (Show)

-- TODO
type Beam = Void

deriveNoun ''Demi


-- Exceptions ------------------------------------------------------------------

data SerfExn
  = UnexpectedPlea Noun Text
  | BadPleaAtom Atom
  | BadPleaNoun Noun [Text] Text
  | PeekBail Goof
  | SerfConnectionClosed
  | SerfHasShutdown
  | BailDuringReplay EventId [Goof]
  | SwapDuringReplay EventId Mug (Wen, Noun) FX
  | SerfNotRunning
  | MissingBootEventsInEventLog Word Word
  | SnapshotAheadOfLog EventId EventId
  | BailDuringWyrd [Goof]
  | SwapDuringWyrd Mug (Wen, Noun) FX
 deriving (Show, Exception)


-- Instances -------------------------------------------------------------------

deriveNoun ''RipeInfo
deriveNoun ''SerfInfo
deriveNoun ''SerfState
