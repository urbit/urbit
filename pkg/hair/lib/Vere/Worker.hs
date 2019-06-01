module Vere.Worker where

import ClassyPrelude
import Control.Lens
import Data.Void

import System.Exit (ExitCode)

import Data.Noun
import Data.Noun.Atom
import Data.Noun.Jam
import Data.Noun.Poet
import Data.Noun.Pill
import Vere.Pier.Types
import System.Process

data Worker = Worker
  { sendHandle :: Handle
  , recvHandle :: Handle
  , process    :: ProcessHandle

  -- , getInput   :: STM (Writ ())
  -- , onComputed :: Writ [Effect] -> STM ()

--  , onExit :: Worker -> IO ()
--  , task       :: Async ()
  }

newtype Cord = Cord ByteString
  deriving newtype (Eq, Ord, Show)

--------------------------------------------------------------------------------

instance ToNoun Cord where
  toNoun (Cord bs) = Atom (bs ^. from (pill . pillBS))

instance FromNoun Cord where
  parseNoun n = do
    atom <- parseNoun n
    pure $ Cord (atom ^. pill . pillBS)

--------------------------------------------------------------------------------

start ::  IO Worker
start = do
  -- Think about how to handle process exit
  -- Tear down subprocess on exit? (terminiteProcess)
  (Just stdin, Just stdout, _, ph) <-
    createProcess (proc "urbit-worker" []){ std_in  = CreatePipe,
                                            std_out = CreatePipe }
  pure (Worker stdin stdout ph)

kill :: Worker -> IO ExitCode
kill w = do
  terminateProcess (process w)
  waitForProcess (process w)

work :: Word64 -> Jam -> Atom
work id (Jam a) = jam $ toNoun (Cord "work", id, a)

data Job = Job Void
  deriving (Eq, Show)

data Tank = Tank Void
  deriving (Eq, Show)

type EventId = Word64

newtype Ship = Ship Word64 -- @p
  deriving newtype (Eq, Show, FromNoun, ToNoun)

data ShipId = ShipId { addr :: Ship, fake :: Bool }
  deriving (Eq, Show)

--------------------------------------------------------------------------------

data Play
    = PlayNone                         --  ~
    | PlaySnap EventId Mug ShipId      --  [@ @ @ ?]
  deriving (Eq, Show)

instance ToNoun Play where
  toNoun = \case PlayNone                  -> Atom 0
                 PlaySnap e m (ShipId a f) -> toNoun (e, m, a, f)

instance FromNoun Play where
  parseNoun = undefined

--------------------------------------------------------------------------------

data Plea
    = Play Play
    | Work EventId Mug Job
    | Done EventId Mug [Ovum]
    | Stdr EventId Cord
    | Slog EventId Word32 Tank
  deriving (Eq, Show)

instance FromNoun Plea where
  parseNoun = undefined

--------------------------------------------------------------------------------

type CompletedEventId = Word64
type NextEventId = Word64

type LogState = Maybe EventId

type WorkerState = (EventId, Mug)

type ReplacementEv = (EventId, Mug, Job)
type WorkResult    = (EventId, Mug, [Ovum])
type WorkerResp    = (Either ReplacementEv WorkResult)

-- Exceptions ------------------------------------------------------------------

data WorkerExn
    = BadComputeId EventId WorkResult
    | BadReplacementId EventId ReplacementEv
    | UnexpectedPlay EventId Play
    | BadPleaAtom Atom
    | BadPleaNoun Noun
  deriving (Show)

instance Exception WorkerExn

-- Utils -----------------------------------------------------------------------

printTank :: Word32 -> Tank -> IO ()
printTank pri t = print "tank"

guardExn :: Exception e => Bool -> e -> IO ()
guardExn ok = unless ok . throwIO

fromJustExn :: Exception e => Maybe a -> e -> IO a
fromJustExn Nothing  exn = throwIO exn
fromJustExn (Just x) exn = pure x

--------------------------------------------------------------------------------

boot :: a -> IO b
boot = undefined

sendAndRecv :: Worker -> EventId -> Atom -> IO WorkerResp
sendAndRecv w eventId event =
  do
    sendAtom w $ work eventId (Jam event)
    loop
  where
    produce :: WorkResult -> IO WorkerResp
    produce (i, m, o) = do
      guardExn (i /= eventId) (BadComputeId eventId (i, m, o))
      pure $ Right (i, m, o)

    replace :: ReplacementEv -> IO WorkerResp
    replace (i, m, j) = do
      guardExn (i /= eventId) (BadReplacementId eventId (i, m, j))
      pure (Left (i, m, j))

    loop :: IO WorkerResp
    loop = recvPlea w >>= \case
      Play p       -> throwIO (UnexpectedPlay eventId p)
      Done i m o   -> produce (i, m, o)
      Work i m j   -> replace (i, m, j)
      Stdr _ cord  -> print cord >> loop
      Slog _ pri t -> printTank pri t >> loop

sendBootEvent :: Worker -> IO ()
sendBootEvent = do
  undefined

-- the ship is booted, but it is behind. shove events to the worker until it is
-- caught up.
replay :: Worker -> WorkerState -> EventId
       -> (EventId -> Word64 -> IO (Vector (EventId, Atom)))
       -> IO ()
replay w (wid, wmug) lastCommitedId getEvents = do
  when (wid == 1) (sendBootEvent w)

  -- todo: we want to stream these in chunks
  events <- getEvents wid (1 + lastCommitedId - wid)

  for_ events $ \(eventId, event) -> do
    (Right (i, mug, ovum)) <- sendAndRecv w eventId event
    undefined

    -- todo: these actually have to happen concurrently



playWorkerState :: Play -> WorkerState
playWorkerState = \case
  PlayNone       -> (1, Mug 0)
  PlaySnap e m _ -> (e, m)

-- computeThread :: Worker -> IO ()
-- computeThread w = start
--   where
--     start = do
--       Just (Play p) <- recvPlea w
--       let (eventId, mug) = playWorkerState p
--       -- fuck it, we'll do it liv_o

--     boot :: WorkerState ->  -> IO ()
--     boot w = do

--       writ <- atomically $ (getInput w)
--       sendAtom w (work (eventId writ) (event writ))


-- The flow here is that we start the worker and then we receive a play event
-- with the current worker state:
--
--  <- [%play ...]
--
-- Base on this, the main flow is 
--

  --  [%work ] ->
  --  <- [%slog]
  --  <- [%slog]
  --  <- [%slog]
  --  <- [%work crash=tang]
  --  [%work ] ->  (replacement)
  --  <- [%slog]
  --  <- [%done]
--    [%work eventId mat]

--  response <- recvAtom w











-- Basic Send and Receive Operations -------------------------------------------

sendAtom :: Worker -> Atom -> IO ()
sendAtom w a = hPut (sendHandle w) (unpackAtom a)

atomBytes :: Iso' Atom ByteString
atomBytes = pill . pillBS

packAtom = view (from atomBytes)

unpackAtom :: Atom -> ByteString
unpackAtom = view atomBytes

recvLen :: Worker -> IO Word64
recvLen = undefined

recvBytes :: Worker -> Word64 -> IO ByteString
recvBytes = undefined

recvAtom :: Worker -> IO (Atom)
recvAtom w = do
  len <- recvLen w
  bs <- recvBytes w len
  pure (packAtom bs)

recvPlea :: Worker -> IO Plea
recvPlea w = do
  a <- recvAtom w
  n <- fromJustExn (cue a)      (BadPleaAtom a)
  p <- fromJustExn (fromNoun n) (BadPleaNoun n)
  pure p
