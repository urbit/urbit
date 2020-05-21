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

import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr           (castPtr)
import Foreign.Storable      (peek, poke)
import System.Process        (ProcessHandle)
import Urbit.Time            (Wen)

import qualified Data.ByteString        as BS
import qualified Data.ByteString.Unsafe as BS


-- IPC Types -------------------------------------------------------------------

type Gang = Maybe (HoonSet Ship)

type Goof = (Term, [Tank])

data Live
  = LExit Atom
  | LSave EventId
  | LPack EventId
 deriving (Show)

type PlayBail = (EventId, Mug, Goof)

data Play
  = PDone Mug
  | PBail PlayBail
 deriving (Show)

data Work
  = WDone EventId Mug [Ef]
  | WSwap EventId Mug (Wen, Noun) [Ef]
  | WBail [Goof]
 deriving (Show)

data Writ
  = WLive Live
  | WPeek Wen Gang Path
  | WPlay EventId [Noun]
  | WWork Wen Ev
 deriving (Show)

data RipeInfo = RipeInfo
  { riProt :: Atom
  , riHoon :: Atom
  , riNock :: Atom
  }
 deriving (Show)

data SerfInfo = SerfInfo
  { siRipe :: RipeInfo
  , siEvId :: EventId
  , siHash :: Mug
  }
 deriving (Show)

type Slog = (Atom, Tank)

data Plea
  = PLive ()
  | PRipe SerfInfo
  | PSlog Slog
  | PPeek (Maybe (Term, Noun))
  | PPlay Play
  | PWork Work
 deriving (Show)

deriveNoun ''Live
deriveNoun ''Play
deriveNoun ''Work
deriveNoun ''Writ
deriveNoun ''RipeInfo
deriveNoun ''SerfInfo
deriveNoun ''Plea

data Serf = Serf
  { serfSend :: Handle
  , serfRecv :: Handle
  , serfProc :: ProcessHandle
  , serfSlog :: Slog -> IO ()
  }


-- API Types -------------------------------------------------------------------

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


-- Exceptions ------------------------------------------------------------------

data SerfExn
--  = BadComputeId EventId WorkResult
--  | BadReplacementId EventId ReplacementEv
--  | UnexpectedPlay EventId (EventId, Mug)
    = UnexpectedPlea Plea Text
    | BadPleaAtom Atom
    | BadPleaNoun Noun [Text] Text
--  | ReplacedEventDuringReplay EventId ReplacementEv
--  | ReplacedEventDuringBoot   EventId ReplacementEv
--  | EffectsDuringBoot         EventId FX
    | SerfConnectionClosed
--  | UnexpectedPleaOnNewShip Plea
--  | InvalidInitialPlea Plea
  deriving (Show, Exception)


-- Low Level IPC Functions -----------------------------------------------------

fromRightExn :: (Exception e, MonadIO m) => Either a b -> (a -> e) -> m b
fromRightExn (Left m)  exn = throwIO (exn m)
fromRightExn (Right x) _   = pure x

withWord64AsByteString :: Word64 -> (ByteString -> IO a) -> IO a
withWord64AsByteString w k = alloca $ \wp -> do
  poke wp w
  bs <- BS.unsafePackCStringLen (castPtr wp, 8)
  k bs

sendLen :: Serf -> Int -> IO ()
sendLen s i = do
  w <- evaluate (fromIntegral i :: Word64)
  withWord64AsByteString (fromIntegral i) (hPut (serfSend s))

sendBytes :: Serf -> ByteString -> IO ()
sendBytes s bs = handle onIOError $ do
  sendLen s (length bs)
  hPut (serfSend s) bs
  hFlush (serfSend s)
 where
  onIOError :: IOError -> IO ()
  onIOError = const (throwIO SerfConnectionClosed)

recvBytes :: Serf -> Word64 -> IO ByteString
recvBytes serf = io . BS.hGet (serfRecv serf) . fromIntegral

recvLen :: Serf -> IO Word64
recvLen w = do
  bs <- BS.hGet (serfRecv w) 8
  case length bs of
    8 -> BS.unsafeUseAsCString bs (peek . castPtr)
    _ -> throwIO SerfConnectionClosed

recvAtom :: Serf -> IO Atom
recvAtom w = do
  len <- recvLen w
  bytesAtom <$> recvBytes w len


-- Send Writ / Recv Plea -------------------------------------------------------

sendWrit :: Serf -> Writ -> IO ()
sendWrit s w = do
  sendBytes s $ jamBS $ toNoun w

recvPlea :: Serf -> IO Plea
recvPlea w = do
  a <- recvAtom w
  n <- fromRightExn (cue a) (const $ BadPleaAtom a)
  p <- fromRightExn (fromNounErr n) (\(p, m) -> BadPleaNoun n p m)
  pure p

recvPleaHandlingSlog :: Serf -> IO Plea
recvPleaHandlingSlog serf = loop
 where
  loop = recvPlea serf >>= \case
    PSlog info -> serfSlog serf info >> loop
    other      -> pure other


-- Higher-Level IPC Functions --------------------------------------------------

recvPlay :: Serf -> IO Play
recvPlay serf = recvPleaHandlingSlog serf >>= \case
  PPlay play -> pure play
  plea       -> throwIO (UnexpectedPlea plea "expecting %play")

recvLive :: Serf -> IO ()
recvLive serf = recvPleaHandlingSlog serf >>= \case
  PLive () -> pure ()
  plea     -> throwIO (UnexpectedPlea plea "expecting %live")

recvWork :: Serf -> IO Work
recvWork serf = do
  recvPleaHandlingSlog serf >>= \case
    PWork work -> pure work
    plea       -> throwIO (UnexpectedPlea plea "expecting %work")

recvPeek :: Serf -> IO (Maybe (Term, Noun))
recvPeek serf = do
  recvPleaHandlingSlog serf >>= \case
    PPeek peek -> pure peek
    plea       -> throwIO (UnexpectedPlea plea "expecting %peek")


-- Request-Response Points -----------------------------------------------------

snapshot :: Serf -> EventId -> IO ()
snapshot serf eve = do
  sendWrit serf (WLive $ LSave eve)
  recvLive serf

compact :: Serf -> EventId -> IO ()
compact serf eve = do
  sendWrit serf (WLive $ LPack eve)
  recvLive serf

scry :: Serf -> Wen -> Gang -> Path -> IO (Maybe (Term, Noun))
scry serf w g p = do
  sendWrit serf (WPeek w g p)
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
  sendWrit serf (WLive $ LExit exitCode)
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
      io $ sendWrit serf (WPlay eid [no])
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
      io (sendWrit serf (WWork wen evn))
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
