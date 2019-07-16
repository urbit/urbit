{-
    - TODO: `Serf` type should have something like:

      ```
      getInput   :: STM (Writ ())
      onComputed :: Writ [Effect] -> STM ()
      onExit     :: Serf -> IO ()
      task       :: Async ()
      ```

    - TODO: `recvLen` is not big-endian safe.
-}

{-# OPTIONS_GHC -Wwarn #-}

module Vere.Serf where

import ClassyPrelude
import Control.Lens

import Data.Void
import Noun
import System.Process
import Vere.Pier.Types

import Control.Concurrent     (threadDelay)
import Data.ByteString        (hGet)
import Data.ByteString.Unsafe (unsafeUseAsCString)
import Foreign.Marshal.Alloc  (alloca)
import Foreign.Ptr            (castPtr)
import Foreign.Storable       (peek, poke)
import System.Exit            (ExitCode)

import qualified Data.ByteString.Unsafe as BS
import qualified Data.Text              as T
import qualified Urbit.Time             as Time


-- Types -----------------------------------------------------------------------

data Serf = Serf
  { sendHandle :: Handle
  , recvHandle :: Handle
  , process    :: ProcessHandle
  }

newtype Job = Job Void
  deriving newtype (Eq, Show, ToNoun, FromNoun)

type EventId = Word64

data Order
    = OBoot LogIdentity
    | OExit Word8
    | OSave EventId
    | OWork EventId Atom
  deriving (Eq, Ord)

type Play = Maybe (EventId, Mug, ShipId)

data Plea
    = Play Play
    | Work EventId Mug Job
    | Done EventId Mug [(Path, Eff)]
    | Stdr EventId Cord
    | Slog EventId Word32 Tank
  deriving (Eq, Show)

type GetEvents = EventId -> Word64 -> IO (Vector (EventId, Atom))

type CompletedEventId = Word64
type NextEventId      = Word64
type SerfState        = (EventId, Mug)
type ReplacementEv    = (EventId, Mug, Job)
type WorkResult       = (EventId, Mug, [(Path, Eff)])
type SerfResp         = (Either ReplacementEv WorkResult)

data SerfExn
    = BadComputeId EventId WorkResult
    | BadReplacementId EventId ReplacementEv
    | UnexpectedPlay EventId Play
    | BadPleaAtom Atom
    | BadPleaNoun Noun Text
    | ReplacedEventDuringReplay EventId ReplacementEv
    | ReplacedEventDuringBoot   EventId ReplacementEv
    | EffectsDuringBoot         EventId [(Path, Eff)]
    | SerfConnectionClosed
    | UnexpectedPleaOnNewShip Plea
    | InvalidInitialPlea Plea
  deriving (Show)


-- Instances -------------------------------------------------------------------

instance Exception SerfExn

-- XX TODO Support prefixes in deriveNoun
instance ToNoun Order where
  toNoun (OBoot id)  = toNoun (Cord "boot", id)
  toNoun (OExit cod) = toNoun (Cord "exit", cod)
  toNoun (OSave id)  = toNoun (Cord "save", id)
  toNoun (OWork w a) = toNoun (Cord "work", w, a)

instance Show Order where
  show = show . toNoun

deriveNoun ''Plea


-- Utils -----------------------------------------------------------------------

printTank :: Word32 -> Tank -> IO ()
printTank pri t = print "[SERF] tank"

guardExn :: Exception e => Bool -> e -> IO ()
guardExn ok = unless ok . throwIO

fromJustExn :: Exception e => Maybe a -> e -> IO a
fromJustExn Nothing  exn = throwIO exn
fromJustExn (Just x) exn = pure x

fromRightExn :: Exception e => Either a b -> (a -> e) -> IO b
fromRightExn (Left m)  exn = throwIO (exn m)
fromRightExn (Right x) _   = pure x


-- Process Management ----------------------------------------------------------

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
kill serf = do
  terminateProcess (process serf)
  waitForProcess (process serf)


-- Basic Send and Receive Operations -------------------------------------------

withWord64AsByteString :: Word64 -> (ByteString -> IO a) -> IO a
withWord64AsByteString w k = do
  alloca $ \wp -> do
    poke wp w
    bs <- BS.unsafePackCStringLen (castPtr wp, 8)
    k bs

sendLen :: Serf -> Int -> IO ()
sendLen s i = do
  w <- evaluate (fromIntegral i :: Word64)
  withWord64AsByteString (fromIntegral i) (hPut (sendHandle s))

sendOrder :: Serf -> Order -> IO ()
sendOrder w o = do
  traceM ("[DEBUG] Serf.sendOrder: " <> show o)
  sendAtom w $ jam $ toNoun o

sendAtom :: Serf -> Atom -> IO ()
sendAtom s a = do
    let bs = unpackAtom a
    sendLen s (length bs)
    hPut (sendHandle s) bs
    hFlush (sendHandle s)
  where
    unpackAtom :: Atom -> ByteString
    unpackAtom = view atomBytes

recvLen :: Serf -> IO Word64
recvLen w = do
  bs <- hGet (recvHandle w) 8
  case length bs of
    8 -> unsafeUseAsCString bs (peek . castPtr)
    _ -> throwIO SerfConnectionClosed

recvBytes :: Serf -> Word64 -> IO ByteString
recvBytes w = do
  hGet (recvHandle w) . fromIntegral

recvAtom :: Serf -> IO Atom
recvAtom w = do
    len <- recvLen w
    bs <- recvBytes w len
    pure (packAtom bs)
  where
    packAtom :: ByteString -> Atom
    packAtom = view (from atomBytes)

cordString :: Cord -> String
cordString (Cord bs) = unpack $ T.strip $ decodeUtf8 bs


--------------------------------------------------------------------------------

requestSnapshot :: Serf -> SerfState -> IO ()
requestSnapshot serf (lastEv, _) = sendOrder serf (OSave lastEv)

requestShutdown :: Serf -> Word8 -> IO ()
requestShutdown serf code = sendOrder serf (OExit code)

shutdownAndKill :: Serf -> Word8 -> IO ExitCode
shutdownAndKill serf code = do
  requestShutdown serf code
  threadDelay 50000 -- TODO XX Hack ("how to tell when this is done?")
  kill serf

{-
    TODO Find a cleaner way to handle `Stdr` Pleas.
-}
recvPlea :: Serf -> IO Plea
recvPlea w = do
  -- traceM ("[DEBUG] Serf.recvPlea: Waiting")

  a <- recvAtom w
  n <- fromRightExn (cue a) (const $ BadPleaAtom a)
  p <- fromRightExn (fromNounErr n) (BadPleaNoun $ traceShowId n)

  case p of Stdr e msg -> do traceM ("[SERF]\t" <> (cordString msg))
                             recvPlea w
            _          -> do traceM ("[DEBUG] Serf.recvPlea: Got " <> show p)
                             pure p

{-
    Waits for initial plea, and then sends boot IPC if necessary.
-}
handshake :: Serf -> LogIdentity -> IO (EventId, Mug)
handshake serf ident = do
    (eventId, mug) <- recvPlea serf >>= \case
      Play Nothing          -> pure (1, Mug 0)
      Play (Just (e, m, _)) -> pure (e, m)
      x                     -> throwIO (InvalidInitialPlea x)

    when (eventId == 1) $ do
        sendOrder serf (OBoot ident)

    pure (eventId, mug)

sendAndRecv :: Serf -> EventId -> Order -> IO SerfResp
sendAndRecv w eventId order =
  do
    sendOrder w order
    res <- loop
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
      Stdr _ cord  -> do traceM ("[SERF]\t" <> cordString cord)
                         loop
      Slog _ pri t -> printTank pri t >> loop

bootFromSeq :: Serf -> BootSeq -> IO ([(EventId, Atom)], SerfState)
bootFromSeq serf (BootSeq ident nocks ovums) = do
    handshake serf ident >>= \case
        (1, Mug 0) -> pure ()
        _          -> error "ship already booted"

    loop [] 1 (Mug 0) seq

  where
    loop :: [(EventId, Atom)] -> EventId -> Mug -> [Mug -> Time.Wen -> Atom]
         -> IO ([(EventId, Atom)], SerfState)
    loop acc eId lastMug []     = pure (reverse acc, (eId, lastMug))
    loop acc eId lastMug (x:xs) = do
        wen <- Time.now
        let atom  = x lastMug wen
        let order = OWork eId atom
        sendAndRecv serf eId order >>= \case
            Right (id, mug, []) -> loop ((eId, atom) : acc) (eId+1) mug xs
            Left badEv          -> throwIO (ReplacedEventDuringBoot eId badEv)
            Right (id, mug, fx) -> throwIO (EffectsDuringBoot eId fx)

    seq :: [Mug -> Time.Wen -> Atom]
    seq = fmap muckNock nocks <> fmap muckOvum ovums
      where
        muckNock nok mug _   = jam $ toNoun (mug, nok)
        muckOvum ov  mug wen = jam $ toNoun (mug, wen, ov)

{-
    The ship is booted, but it is behind. shove events to the worker
    until it is caught up.

    This will pull events from the event log in batches of 1000.
-}
replayEvents :: Serf
             -> SerfState
             -> LogIdentity
             -> EventId
             -> (EventId -> Word64 -> IO (Vector (EventId, Atom)))
             -> IO (EventId, Mug)
replayEvents w (wid, wmug) ident lastCommitedId getEvents = do
    vLast <- newIORef (wid, wmug)
    loop vLast wid
    readIORef vLast
  where
    loop :: IORef SerfState -> EventId -> IO ()
    loop vLast curEvent = do
      let toRead = min 1000 (1 + lastCommitedId - curEvent)
      when (toRead > 0) $ do
        events <- getEvents curEvent toRead
        for_ events $ \(eventId, event) -> do
          sendAndRecv w eventId (OWork eventId event) >>= \case
            Left ev            -> throwIO (ReplacedEventDuringReplay eventId ev)
            Right (id, mug, _) -> writeIORef vLast (id, mug)
        loop vLast (curEvent + toRead)

replay :: Serf -> LogIdentity -> EventId -> GetEvents -> IO (EventId, Mug)
replay serf ident lastEv getEvents = do
    ws <- handshake serf ident
    replayEvents serf ws ident lastEv getEvents


-- Compute Thread --------------------------------------------------------------

startComputeThread :: Serf -> STM Ovum -> (EventId, Mug) -> IO (Async ())
startComputeThread w getEvent (evendId, mug) = async $ forever $ do
  ovum <- atomically $ getEvent

  currentDate <- Time.now

  let _mat = jam (undefined (mug, currentDate, ovum))

  undefined
