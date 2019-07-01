module Vere.Serf where

import ClassyPrelude
import Control.Lens
import Data.Void

import Data.Noun
import Data.Noun.Atom
import Data.Noun.Jam hiding (jam)
import Data.Noun.Jam.Put (jam, jamBS)
import Data.Noun.Poet
import Data.Noun.Pill
import Vere.Pier.Types
import System.Process

import Foreign.Marshal.Alloc (alloca)
import System.Exit (ExitCode)
import Data.ByteString (hGet)
import Data.ByteString.Unsafe (unsafeUseAsCString)
import Foreign.Ptr (castPtr)
import Foreign.Storable (poke, peek)

import qualified Data.ByteString.Unsafe as BS
import qualified Urbit.Time             as Time
import qualified Vere.Log               as Log


--------------------------------------------------------------------------------


{-
    TODO:
      - getInput   :: STM (Writ ())
      - onComputed :: Writ [Effect] -> STM ()
      - onExit     :: Serf -> IO ()
      - task       :: Async ()
-}
data Serf = Serf
  { sendHandle :: Handle
  , recvHandle :: Handle
  , process    :: ProcessHandle
  }


--------------------------------------------------------------------------------

{-
    TODO Think about how to handle process exit
    TODO Tear down subprocess on exit? (terminiteProcess)
    TODO `config` is a stub, fill it in.
-}
startSerfProcess :: FilePath -> IO Serf
startSerfProcess pier =
  do
    (Just i, Just o, _, p) <- createProcess pSpec
    pure (Serf i o p)
  where
    chkDir  = traceShowId pier
    diskKey = ""
    config  = "0"
    args    = [chkDir, diskKey, config]
    pSpec   = (proc "urbit-worker" args)
                { std_in = CreatePipe
                , std_out = CreatePipe
                }

kill :: Serf -> IO ExitCode
kill w = do
  terminateProcess (process w)
  waitForProcess (process w)

work :: Word64 -> Jam -> Atom
work id (Jam a) = jam $ toNoun (Cord "work", id, a)

newtype Job = Job Void
  deriving newtype (Eq, Show, ToNoun, FromNoun)

type EventId = Word64

newtype Ship = Ship Word64 -- @p
  deriving newtype (Eq, Ord, Show, ToNoun, FromNoun)

newtype ShipId = ShipId (Ship, Bool)
  deriving newtype (Eq, Ord, Show, ToNoun, FromNoun)

--------------------------------------------------------------------------------

type Play = Maybe (EventId, Mug, ShipId)

data Plea
    = Play Play
    | Work EventId Mug Job
    | Done EventId Mug [(Path, Eff)]
    | Stdr EventId Cord
    | Slog EventId Word32 Tank
  deriving (Eq, Show)

instance ToNoun Plea where
  toNoun = \case
    Play p     -> toNoun (Cord "play", p)
    Work i m j -> toNoun (Cord "work", i, m, j)
    Done i m o -> toNoun (Cord "done", i, m, o)
    Stdr i msg -> toNoun (Cord "stdr", i, msg)
    Slog i p t -> toNoun (Cord "slog", i, p, t)

instance FromNoun Plea where
  parseNoun n =
    parseNoun n >>= \case
      (Cord "play", p) -> parseNoun p <&> \p         -> Play p
      (Cord "work", w) -> parseNoun w <&> \(i, m, j) -> Work i m j
      (Cord "done", d) -> parseNoun d <&> \(i, m, o) -> Done i m o
      (Cord "stdr", r) -> parseNoun r <&> \(i, msg)  -> Stdr i msg
      (Cord "slog", s) -> parseNoun s <&> \(i, p, t) -> Slog i p t
      (Cord tag   , s) -> fail ("Invalid plea tag: " <> unpack (decodeUtf8 tag))

--------------------------------------------------------------------------------

type CompletedEventId = Word64
type NextEventId = Word64

type SerfState = (EventId, Mug)

type ReplacementEv = (EventId, Mug, Job)
type WorkResult  = (EventId, Mug, [(Path, Eff)])
type SerfResp    = (Either ReplacementEv WorkResult)

-- Exceptions ------------------------------------------------------------------

data SerfExn
    = BadComputeId EventId WorkResult
    | BadReplacementId EventId ReplacementEv
    | UnexpectedPlay EventId Play
    | BadPleaAtom Atom
    | BadPleaNoun Noun Text
    | ReplacedEventDuringReplay EventId ReplacementEv
    | SerfConnectionClosed
    | UnexpectedPleaOnNewShip Plea
    | InvalidInitialPlea Plea
  deriving (Show)

instance Exception SerfExn

-- Utils -----------------------------------------------------------------------

printTank :: Word32 -> Tank -> IO ()
printTank pri t = print "[SERF] tank"

guardExn :: Exception e => Bool -> e -> IO ()
guardExn ok = unless ok . throwIO

fromJustExn :: Exception e => Maybe a -> e -> IO a
fromJustExn Nothing  exn = throwIO exn
fromJustExn (Just x) exn = pure x

fromRightExn :: Exception e => Either Text a -> (Text -> e) -> IO a
fromRightExn (Left m)  exn = throwIO (exn m)
fromRightExn (Right x) _   = pure x

--------------------------------------------------------------------------------

sendAndRecv :: Serf -> EventId -> Atom -> IO SerfResp
sendAndRecv w eventId event =
  do
    traceM ("sendAndRecv: " <> show eventId)

    -- traceM ("<cue>")
    -- traceM (maybe "bad cue" showNoun $ cue event)
    -- traceM ("</cue>")

    traceM ("<jam>")
    wEv <- evaluate $ force $ work eventId (Jam event)
    traceM ("</jam>")

    sendAtom w wEv
    res <- loop
    traceM ("sendAndRecv.done " <> show res)
    pure res
  where
    produce :: WorkResult -> IO SerfResp
    produce (i, m, o) = do
      guardExn (i == eventId) (BadComputeId eventId (i, m, o))
      pure $ Right (i, m, o)

    replace :: ReplacementEv -> IO SerfResp
    replace (i, m, j) = do
      guardExn (i == eventId) (BadReplacementId eventId (i, m, j))
      pure (Left (i, m, j))

    loop :: IO SerfResp
    loop = recvPlea w >>= \case
      Play p       -> throwIO (UnexpectedPlay eventId p)
      Done i m o   -> produce (i, m, o)
      Work i m j   -> replace (i, m, j)
      Stdr _ cord  -> putStrLn (pack ("[SERF] " <> cordString cord)) >> loop
      Slog _ pri t -> printTank pri t >> loop

sendBootEvent :: LogIdentity -> Serf -> IO ()
sendBootEvent id w = do
  sendAtom w $ jam $ toNoun (Cord "boot", id)


-- the ship is booted, but it is behind. shove events to the worker until it is
-- caught up.
replayEvents :: Serf
             -> SerfState
             -> LogIdentity
             -> EventId
             -> (EventId -> Word64 -> IO (Vector (EventId, Atom)))
             -> IO (EventId, Mug)
replayEvents w (wid, wmug) identity lastCommitedId getEvents = do
  traceM ("replayEvents: " <> show wid <> " " <> show wmug)

  when (wid == 1) (sendBootEvent identity w)

  vLast <- newIORef (wid, wmug)
  loop vLast wid

  res <- readIORef vLast
  traceM ("replayEvents.return " <> show res)
  pure res

  where
    -- Replay events in batches of 1000.
    loop vLast curEvent = do
      traceM ("replayEvents.loop: " <> show curEvent)
      let toRead = min 1000 (1 + lastCommitedId - curEvent)
      when (toRead > 0) do
        traceM ("replayEvents.loop.getEvents " <> show toRead)

        events <- getEvents curEvent toRead

        traceM ("got events " <> show (length events))

        for_ events $ \(eventId, event) -> do
          sendAndRecv w eventId event >>= \case
            Left ev -> throwIO (ReplacedEventDuringReplay eventId ev)
            Right (id, mug, _) -> writeIORef vLast (id, mug)

        loop vLast (curEvent + toRead)


bootSerf :: Serf -> LogIdentity -> Pill -> IO (EventId, Mug)
bootSerf w ident pill =
  do
    recvPlea w >>= \case
      Play Nothing -> pure ()
      x@(Play _)   -> throwIO (UnexpectedPleaOnNewShip x)
      x            -> throwIO (InvalidInitialPlea x)

    -- TODO: actually boot the pill
    undefined

    -- Maybe return the current event id ? But we'll have to figure that out
    -- later.
    pure undefined

type GetEvents = EventId -> Word64 -> IO (Vector (EventId, Atom))

replay :: Serf -> LogIdentity -> EventId -> GetEvents -> IO (EventId, Mug)
replay w ident lastEv getEvents = do
    ws@(eventId, mug) <- recvPlea w >>= \case
      Play Nothing          -> pure (1, Mug 0)
      Play (Just (e, m, _)) -> pure (e, m)
      x                     -> throwIO (InvalidInitialPlea x)

    traceM ("got plea! " <> show eventId <> " " <> show mug)

    replayEvents w ws ident lastEv getEvents

workerThread :: Serf -> STM Ovum -> (EventId, Mug) -> IO (Async ())
workerThread w getEvent (evendId, mug) = async $ forever do
  ovum <- atomically $ getEvent

  currentDate <- Time.now

  let mat = jam (undefined (mug, currentDate, ovum))

  undefined

  -- Writ (eventId + 1) Nothing mat
  -- -- assign a new event id.
  -- -- assign a date
  -- -- get current mug state
  -- -- (jam [mug event])
  -- sendAndRecv

requestSnapshot :: Serf -> IO ()
requestSnapshot w =  undefined

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

withWord64AsByteString :: Word64 -> (ByteString -> IO a) -> IO a
withWord64AsByteString w k = do
  alloca $ \wp -> do
    poke wp w
    bs <- BS.unsafePackCStringLen (castPtr wp, 8)
    k bs

sendLen :: Serf -> Int -> IO ()
sendLen s i = do
  traceM "sendLen.put"
  w <- evaluate (fromIntegral i :: Word64)
  withWord64AsByteString (fromIntegral i) (hPut (sendHandle s))
  traceM "sendLen.done"

sendAtom :: Serf -> Atom -> IO ()
sendAtom s a = do
  traceM "sendAtom"
  let bs = unpackAtom a
  sendLen s (length bs)
  hPut (sendHandle s) bs
  hFlush (sendHandle s)
  traceM "sendAtom.return ()"

atomBytes :: Iso' Atom ByteString
atomBytes = pill . pillBS

packAtom = view (from atomBytes)

unpackAtom :: Atom -> ByteString
unpackAtom = view atomBytes

recvLen :: Serf -> IO Word64
recvLen w = do
  traceM "recvLen.wait"
  bs <- hGet (recvHandle w) 8
  traceM "recvLen.got"
  case length bs of
    -- This is not big endian safe
    8 -> unsafeUseAsCString bs (peek . castPtr)
    _ -> throwIO SerfConnectionClosed

recvBytes :: Serf -> Word64 -> IO ByteString
recvBytes w = do
  traceM "recvBytes"
  hGet (recvHandle w) . fromIntegral

recvAtom :: Serf -> IO Atom
recvAtom w = do
  traceM "recvAtom"
  len <- recvLen w
  bs <- recvBytes w len
  pure (packAtom bs)

cordString :: Cord -> String
cordString (Cord bs) = unpack $ decodeUtf8 bs

recvPlea :: Serf -> IO Plea
recvPlea w = do
  traceM "recvPlea"

  a <- recvAtom w
  traceM ("recvPlea.cue " <> show (length $ a ^. atomBytes))
  n <- fromJustExn (cue a)      (BadPleaAtom a)
  traceM "recvPlea.doneCue"
  p <- fromRightExn (fromNounErr n) (BadPleaNoun (trace (showNoun n) n))

  traceM "recvPlea.done"

  -- TODO Hack!
  case p of
    Stdr e msg -> traceM ("[SERF] " <> cordString msg) >> recvPlea w
    _          -> pure p
