module Urbit.Vere.Serf.Types where

import Urbit.Prelude

import Urbit.Arvo      (Desk, Ev, FX, Wynn)
import Urbit.Noun.Time (Wen)


-- Types -----------------------------------------------------------------------

type EventId = Word64

type PlayBail = (EventId, Mug, Goof)

type Slog = (Atom, Tank)

data Vers = Vers Wynn
  deriving (Show)

data Self = Self
  { who    :: Ship
  , isFake :: Bool
  }
  deriving (Eq, Ord, Show)

data SerfState = SerfState
  { ssLast :: !EventId
  , ssHash :: !Mug
  }
 deriving (Show, Eq)

data Ripe = Ripe
  { riVers :: Vers
  , riSelf :: Self
  , riStat :: SerfState
  }
  deriving (Show)

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
  | RunOkay FX

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
  = UnexpectedGift Noun Text
  | BadGiftAtom Atom
  | BadGiftNoun Noun [Text] Text
  | PeekBail Goof
  | SerfConnectionClosed
  | SerfHasShutdown
  | SerfNotRunning
  | BailDuringWyrd [Goof]
  | SwapDuringWyrd Mug (Wen, Noun) FX
 deriving (Show, Exception)


-- Instances -------------------------------------------------------------------

instance ToNoun Vers where
  toNoun (Vers w) = C (A 2) $ toNoun w

instance FromNoun Vers where
  parseNoun = \case
    C (A 2) w -> Vers <$> parseNoun w
    v -> error ("Unexpected version in %ripe: " <> show v)

deriveNoun ''Self
deriveNoun ''SerfState
deriveNoun ''Ripe
