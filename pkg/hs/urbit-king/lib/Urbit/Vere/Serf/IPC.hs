{-
|%
::  +writ: from king to serf
::
+$  writ
  $%  $:  %live
          $%  [%exit cod=@]
              [%save eve=@]
              [%pack eve=@]
      ==  ==
      [%peek now=date lyc=gang pat=path]
      [%play eve=@ lit=(list ?((pair date ovum) *))]
      [%work job=(pair date ovum)]
  ==
::  +plea: from serf to king
::
+$  plea
  $%  [%live ~]
      [%ripe [pro=@ hon=@ nok=@] eve=@ mug=@]
      [%slog pri=@ ?(cord tank)]
      [%peek dat=(unit (cask))]
      $:  %play
          $%  [%done mug=@]
              [%bail eve=@ mug=@ dud=goof]
      ==  ==
      $:  %work
          $%  [%done eve=@ mug=@ fec=(list ovum)]
              [%swap eve=@ mug=@ job=(pair date ovum) fec=(list ovum)]
              [%bail lud=(list goof)]
      ==  ==
  ==
--
-}

module Urbit.Vere.Serf.IPC where

import Urbit.Prelude hiding ((<|))

import Data.Conduit
import Urbit.Arvo
import Urbit.Vere.Pier.Types hiding (Work)

import System.Process (ProcessHandle)
import Urbit.Time     (Wen)


-- Types -----------------------------------------------------------------------

type Gang = Maybe (HoonSet Ship)

type Goof = (Term, [Tank])

data Live
  = LExit Atom
  | LSave EventId
  | LPack EventId

type PlayBail = (EventId, Mug, Goof)

data Play
  = PDone Mug
  | PBail PlayBail

data Work
  = WDone EventId Mug [Ef]
  | WSwap EventId Mug (Wen, Noun) [Ef]
  | WBail [Goof]

data Writ
  = WLive Live
  | WPeek Wen Gang Path
  | WPlay EventId [Noun]
  | WWork Wen Ev

data RipeInfo = RipeInfo
  { riProt :: Atom
  , riHoon :: Atom
  , riNock :: Atom
  }

data SerfInfo = SerfInfo
  { siRipe :: RipeInfo
  , siEvId :: EventId
  , siHash :: Mug
  }

data Plea
  = PLive ()
  | PRipe SerfInfo
  | PSlog Atom Tank
  | PPeek (Maybe (Term, Noun))
  | PPlay Play
  | PWork Work

deriveNoun ''Live
deriveNoun ''Play
deriveNoun ''Work
deriveNoun ''Writ
deriveNoun ''RipeInfo
deriveNoun ''SerfInfo
deriveNoun ''Plea

{-
  startup:
    wait for `PRipe`

  replay:
    send WPlay
    wait for PPlay
      crash on PRipe or PWork
    (maybe send LSave, LPack, LExit)
    (print slogs)

  running:
    Send WLive or WWork
    wait for PWork
      crash on PRipe or PPlay
    (maybe send LSave, LPack, LExit)
    (print slogs)
    crash on
-}

data Serf = Serf
  { sendHandle :: Handle
  , recvHandle :: Handle
  , process    :: ProcessHandle
  }

{-
data Lord = Lord
  { serf    :: Serf
  , foo     :: TVar (EventId, Mug)
  , sent    :: TVar (Seq Writ)
  , pending :: TVar (Seq Writ)
  }
-}

data SerfConfig = SerfConfig -- binary, directory, &c

data RunError
  = RunBail [Goof]
  | RunSwap EventId Mug Wen Noun [Ef]

data RunInput
  = RunSnap
  | RunPack
  | RunPeek Wen Gang Path (Maybe (Term, Noun) -> IO ())
  | RunWork Wen Ev (RunError -> IO ())

data RunOutput = RunOutput EventId Mug Wen (Either Noun Ev) [Ef]


-- Low Level IPC Functions -----------------------------------------------------

send :: Serf -> Writ -> IO ()
send = error "TODO"

recv :: Serf -> IO Plea
recv = error "TODO"

recvPlay :: Serf -> IO Play
recvPlay serf = recv serf >>= \case
  PLive ()   -> error "unexpected %live plea."
  PRipe si   -> error "TODO: crash"
  PPeek _    -> error "TODO: crash"
  PWork _    -> error "TODO: crash"
  PPlay play -> pure play
  PSlog a t  -> do
    io $ print (a, t) -- TODO
    recvPlay serf

recvLive :: Serf -> IO ()
recvLive serf = recv serf >>= \case
  PLive ()   -> pure ()
  PRipe si   -> error "TODO: crash"
  PPeek _    -> error "TODO: crash"
  PWork _    -> error "TODO: crash"
  PPlay play -> error "TODO: crash"
  PSlog a t  -> do
    io $ print (a, t) -- TODO
    recvLive serf

-- TODO Should eid just be a mutable var in the serf?
snapshot :: Serf -> EventId -> IO ()
snapshot serf eve = do
  send serf (WLive $ LSave eve)
  recvLive serf

compact :: Serf -> EventId -> IO ()
compact serf eve = do
  send serf (WLive $ LPack eve)
  recvLive serf

recvWork :: Serf -> IO Work
recvWork = error "TODO"

recvPeek :: Serf -> IO (Maybe (Term, Noun))
recvPeek = error "TODO"

scry :: Serf -> Wen -> Gang -> Path -> IO (Maybe (Term, Noun))
scry serf w g p = do
  send serf (WPeek w g p)
  recvPeek serf


-- Serf Usage Flows ------------------------------------------------------------

start :: SerfConfig -> IO (Serf, SerfInfo)
start = error "TODO"

{-
  TODO wait for process exit?
  TODO force shutdown after time period? Not our job?
-}
shutdown :: Serf -> Atom -> IO ()
shutdown serf exitCode = do
  send serf (WLive $ LExit exitCode)
  pure ()

{-
  TODO Take advantage of IPC support for batching.
  TODO Maybe take snapshots
-}
replay
  :: Serf -> SerfInfo -> ConduitT Noun Void IO (Either PlayBail (Mug, EventId))
replay serf info = go (siHash info) (siEvId info)
 where
  go :: Mug -> EventId -> ConduitT Noun Void IO (Either PlayBail (Mug, EventId))
  go mug eid = await >>= \case
    Nothing -> pure (Right (mug, eid))
    Just no -> do
      io $ send serf (WPlay eid [no])
      io (recvPlay serf) >>= \case
        PBail bail -> pure (Left bail)
        PDone hash -> go hash (eid + 1)

{-
  TODO callbacks on snapshot and compaction?
  TODO Take advantage of async IPC to fill pipe with more than one thing.
-}
running :: Serf -> SerfInfo -> ConduitT RunInput RunOutput IO (Mug, EventId)
running serf info = go (siHash info) (siEvId info)
 where
  go mug eve = await >>= \case
    Nothing      -> pure (mug, eve)
    Just RunSnap -> do
      io (snapshot serf eve)
      go mug eve
    Just RunPack -> do
      io (compact serf eve)
      go mug eve
    Just (RunPeek wen gang pax act) -> do
      res <- io (scry serf wen gang pax)
      io (act res)
      go mug eve
    Just (RunWork wen evn err) -> do
      io (send serf (WWork wen evn))
      io (recvWork serf) >>= \case
        WDone eid hash fx -> do
          yield (RunOutput eid hash wen (Right evn) fx)
          go hash eid
        WSwap eid hash (wen, noun) fx -> do
          io $ err (RunSwap eid hash wen noun fx)
          yield (RunOutput eid hash wen (Left noun) fx)
          go hash eid
        WBail goofs -> do
          io $ err (RunBail goofs)
          go mug eve
