module Urbit.Vere.Serf.IPC where

import Urbit.Prelude

import Urbit.Arvo
import Urbit.Vere.Pier.Types hiding (Work)

import Urbit.Time (Wen)

type Gang = (Maybe (HoonSet Ship))

type Goof = (Term, [Tank])

data Live
   = LExit Atom
   | LSave EventId
   | LPack (EventId)

data Play
   = PDone Mug
   | PBail EventId Mug Goof

data Work
   = WDone EventId Mug [Ef]
   | WSwap EventId Mug (Wen, Noun) [Ef]
   | WBail [Goof]

data Writ
   = WLive Live
   | WPeek Wen Gang Path
   | WPlay EventId [Noun]
   | WWork Wen Ev

data Plea
   = PLive ()
   | PRipe (Atom, Atom, Atom) EventId Mug
   | PSlog Atom Tank
   | PPeek (Maybe (Term, Noun))
   | PPlay Play
   | PWork Work

deriveNoun ''Live
deriveNoun ''Play
deriveNoun ''Work
deriveNoun ''Writ
deriveNoun ''Plea

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

data Lord = Lord
  { sendHandle :: Handle
  , recvHandle :: Handle
  , process    :: ProcessHandle
  , foo        :: TVar(EventId, Mug)
  , sent       :: TVar(Seq Writ)
  , pending    :: TVar(Seq Writ)
  }

data SerfConfig = SerfConfig() -- binary, directory, &c
data SerfInfo = SerfInfo
  { siNock :: Atom
  , siHoon :: Atom
  }

start :: SerfConfig -> IO(Lord, SerfInfo)
start = undefined

pack  :: Lord -> IO() -- wait for queue to drain, then send with latest EventId
pack l = atomically $ do
  q <- readTVar(pending l)
  writeTVar(pending l) ((Pack 0) <| q)

