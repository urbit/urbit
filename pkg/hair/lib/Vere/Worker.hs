module Vere.Worker where

import ClassyPrelude
import Control.Lens
import Data.Void

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
  deriving (Eq)

instance Show Cord where
  show (Cord bs) = show bs -- TODO

-------------------------------------

class Poet a where
  _Poet :: Prism' Noun a

toNoun' :: Poet a => a -> Noun
toNoun' = review _Poet

fromNoun' :: Poet a => Noun -> Maybe a
fromNoun' = preview _Poet

instance Poet Cord where
  _Poet = undefined

instance ToNoun Cord where
  toNoun = undefined

-------------------------------------

start ::  IO Worker
start = do
  -- Think about how to handle process exit
  -- Tear down subprocess on exit? (terminiteProcess)
  (Just stdin, Just stdout, _, ph) <-
    createProcess (proc "urbit-worker" []){ std_in  = CreatePipe,
                                            std_out = CreatePipe }
  pure (Worker stdin stdout ph)

kill :: Worker -> IO ()
kill worker = undefined

work :: Word64 -> Jam -> Atom
work id (Jam a) = jam $ toNoun (Cord "work", id, a)

data Job = Job Void
  deriving (Eq, Show)

data Tank = Tank Void
  deriving (Eq, Show)

type EventId = Word64

data Ship = Ship Word64 -- @p
  deriving (Eq, Show)

data ShipId = ShipId { addr :: Ship, fake :: Bool }
  deriving (Eq, Show)

data Play
    = PlayNone                         --  ~
    | PlaySnap EventId Mug ShipId      --  [@ @ @ ?]
  deriving Eq

-- TODO Hack
deriving instance Show Mug
deriving instance Eq Mug
deriving instance Eq Ovum
deriving instance Show Ovum

data Plea
    = Play Play
    | Work EventId Mug Job
    | Done EventId Mug [Ovum]
    | Stdr EventId Cord
    | Slog EventId Word32 Tank
  deriving Eq


type CompletedEventId = Word64
type NextEventId = Word64

type LogState = Maybe EventId

type WorkerState = (EventId, Mug)


-- boot
-- boot = do
--   sendAtom w (

printTank :: Word32 -> Tank -> IO ()
printTank pri t = print "tank"


assertErr = undefined

sendAndRecv :: Worker -> EventId -> Atom -> IO (Either (EventId, Mug, Job) (EventId, Mug, [Ovum]))
sendAndRecv w eventId event =
  do
    sendAtom w (work eventId (Jam event))
    loop
  where
    recv i m o = do
      assertErr (i == eventId) "bad event id in sendAndRecv"
      pure (Right (i, m, o))
    replace i m j = do
      assertErr (i == eventId) "bad replacement id in sendAndRecv"
      pure (Left (i, m, j))
    loop = recvPlea w >>= \case
      Nothing -> error "everything is on fire. i'm sorry."
      Just (Play p) -> error "the state is in the wrong place."
      Just (Done i m o) -> recv i m o
      Just (Work i m j)  -> replace i m j
      Just (Stdr _ cord) -> print cord >> loop
      Just (Slog _ pri t) -> (printTank pri t) >> loop


sendBootEvent = undefined

-- the ship is booted, but it is behind. shove events to the worker until it is
-- caught up.
replay :: Worker -> WorkerState -> EventId
       -> (EventId -> Word64 -> IO (Vector (EventId, Atom)))
       -> IO ()
replay w (wid, wmug) lastCommitedId getEvents = do
  case wid of
    1 -> sendBootEvent
    _ -> pure ()

  -- todo: we want to stream these in chunks
  events <- getEvents wid (1 + lastCommitedId - wid)

  for_ events $ \(eventId, event) -> do
    (Right (i, mug, ovum)) <- sendAndRecv w eventId event
    undefined

    -- todo: these actually have to happen concurrently



playToState :: Play -> WorkerState
playToState = \case
  PlayNone -> (1, Mug 0)
  PlaySnap e m _ -> (e, m)

-- computeThread :: Worker -> IO ()
-- computeThread w = start
--   where
--     start = do
--       Just (Play p) <- recvPlea w
--       let (eventId, mug) = playToState p
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

--  response <- recvAtom w











--------------------------------------------------------------------------------


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

fromNoun :: Noun -> Maybe a
fromNoun = const Nothing -- TODO

recvPlea :: Worker -> IO (Maybe Plea)
recvPlea w = do
  a <- recvAtom w
  pure (cue a >>= fromNoun)

--    [%work eventId mat]
