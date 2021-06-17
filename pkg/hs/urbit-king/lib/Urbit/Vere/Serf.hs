{-|
  Low-Level IPC flows for interacting with the serf process.

  - Serf process can be started and shutdown with `work` and `stop`.
  - You can ask the serf what its last event was with
    `serfLastEventBlocking`.
  - A running serf can be asked to compact its heap or take a snapshot.
  - You can scry into a running serf.
  - A running serf can be sent a boot event.
-}

module Urbit.Vere.Serf
  ( Serf
  , work
  , stop
  , boot
  , run
  , sendSIGINT
  , module Urbit.Vere.Serf.Types
  )
where

import Urbit.Prelude hiding ((<|))

import Data.Bits
import Data.Conduit
import System.Process
import Urbit.Vere.Serf.Types
import Urbit.Vere.Serf.IPC.Types

import Control.Monad.STM            (retry)
import Control.Monad.Trans.Resource (MonadResource, allocate, runResourceT)
import Data.Sequence                (Seq((:<|), (:|>)))
import Foreign.Marshal.Alloc        (alloca)
import Foreign.Ptr                  (castPtr)
import Foreign.Storable             (peek, poke)
import RIO.Prelude                  (decodeUtf8Lenient)
import System.Exit                  (ExitCode)
import System.Posix.Signals         (sigINT, sigKILL, signalProcess)
import Urbit.Arvo                   (FX)
import Urbit.Arvo.Event
import Urbit.Noun.Time              (Wen)

import qualified Data.ByteString        as BS
import qualified Data.ByteString.Unsafe as BS
import qualified System.IO.Error        as IO
import qualified Urbit.Noun.Time        as Time


-- Serf API --------------------------------------------------------------------

data Serf = Serf
  { serfSend :: Handle
  , serfRecv :: Handle
  , serfProc :: ProcessHandle
  , serfSlog :: Slog -> IO ()
  }


-- Low Level IPC Functions -----------------------------------------------------

fromRightExn :: (Exception e, MonadIO m) => Either a b -> (a -> e) -> m b
fromRightExn (Left m)  exn = throwIO (exn m)
fromRightExn (Right x) _   = pure x

-- TODO Support Big Endian
sendLen :: Serf -> Int -> IO ()
sendLen s i = do
  w <- evaluate (fromIntegral i :: Word64)
  withWord64AsByteString w (hPut (serfSend s))
 where
  withWord64AsByteString :: Word64 -> (ByteString -> IO a) -> IO a
  withWord64AsByteString w k = alloca $ \wp -> do
    poke wp w
    bs <- BS.unsafePackCStringLen (castPtr wp, 8)
    k bs

sendBytes :: Serf -> ByteString -> IO ()
sendBytes s bs = handle onIOError $ do
  sendLen s (length bs)
  hPut (serfSend s) bs
  hFlush (serfSend s)
 where
  onIOError :: IOError -> IO ()
  onIOError = const (throwIO SerfConnectionClosed)

recvBytes :: Serf -> Word64 -> IO ByteString
recvBytes serf = BS.hGet (serfRecv serf) . fromIntegral

recvLen :: Serf -> IO Word64
recvLen w = do
  bs <- BS.hGet (serfRecv w) 8
  case length bs of
    8 -> BS.unsafeUseAsCString bs (peek @Word64 . castPtr)
    _ -> throwIO SerfConnectionClosed

recvResp :: Serf -> IO ByteString
recvResp serf = do
  len <- recvLen serf
  recvBytes serf len


-- Send Task / Recv Gift -------------------------------------------------------

sendBoot :: Serf -> Boot -> IO ()
sendBoot s = sendBytes s . jamBS . toNoun

sendTask :: Serf -> Task -> IO ()
sendTask s = sendBytes s . jamBS . toNoun

recvGift :: Serf -> IO Gift
recvGift w = do
  b <- recvResp w
  n <- fromRightExn (cueBS b) (const $ BadGiftAtom $ bytesAtom b)
  p <- fromRightExn (fromNounErr @Gift n) (\(p, m) -> BadGiftNoun n p m)
  pure p

-- | Block, polling the next non-out-of-order gift from the serf.
recvGiftHandleOOOs :: Serf -> IO Gift
recvGiftHandleOOOs serf = loop
 where
  loop = recvGift serf >>= \case
    GSlog info        -> serfSlog serf info >> loop
    GFlog (Cord ofni) -> serfSlog serf (0, Tank $ Leaf $ Tape $ ofni) >> loop
    GSync num mug     -> loop  -- XX
    other             -> pure other


-- Choosily "Accept" Gifts From Serf -------------------------------------------

recvLive :: Serf -> IO ()
recvLive serf = recvGiftHandleOOOs serf >>= \case
  GLive () -> pure ()
  gift     -> throwIO (UnexpectedGift (toNoun gift) "expecting %live")

recvPeek :: Serf -> IO (Maybe (Term, Noun))
recvPeek serf = recvGiftHandleOOOs serf >>= \case
  GPeek (EachYes peek) -> pure peek
  -- XX surface error content ; change `scry` return type to Either Goof
  GPeek (EachNo dud)   -> pure Nothing
  gift                 -> throwIO (UnexpectedGift (toNoun gift) "expecting %peek")

recvPoke :: Serf -> IO (Each FX [Goof])
recvPoke serf = recvGiftHandleOOOs serf >>= \case
  GPoke poke -> pure poke
  gift       -> throwIO (UnexpectedGift (toNoun gift) "expecting %poke")

recvRipe :: Serf -> IO Ripe
recvRipe serf = recvGiftHandleOOOs serf >>= \case
  GRipe ripe -> pure ripe
  gift       -> throwIO (UnexpectedGift (toNoun gift) "expecting %play")


-- Starting the Serf -----------------------------------------------------------

compileFlags :: [Flag] -> Word
compileFlags = foldl' (\acc flag -> setBit acc (fromEnum flag)) 0

readStdErr :: Handle -> (Text -> IO ()) -> IO () -> IO ()
readStdErr h onLine onClose = loop
 where
  loop = do
    IO.tryIOError (BS.hGetLine h >>= onLine . decodeUtf8Lenient) >>= \case
      Left exn -> onClose
      Right () -> loop

start :: String -> Config -> IO Serf
start mode (Config exePax pierPath flags onSlog onStdr onDead) = do
  (Just i, Just o, Just e, p) <- createProcess pSpec
  void $ async (readStdErr e onStdr onDead)
  pure $ Serf i o p onSlog
 where
  diskKey = ""
  config  = show (compileFlags flags)
  rock    = "0"      -- XX support loading from rock
  cache   = "50000"  -- XX support memo-cache size
  args    = [mode, pierPath, diskKey, config, cache]
  pSpec   = (proc exePax args) { std_in  = CreatePipe
                               , std_out = CreatePipe
                               , std_err = CreatePipe
                               }

work :: Config -> IO (Serf, Ripe)
work fig = do
  serf <- start "work" fig
  info <- recvRipe serf
  pure (serf, info)


boot :: Config -> Boot -> IO ExitCode
boot fig boot = do
  serf <- start "boot" fig
  sendBoot serf boot
  tsFlog <- async (loop serf)
  flip onException (cancel tsFlog) $ waitForProcess $ serfProc serf
  where
    loop serf = do
      x <- recvGiftHandleOOOs serf
      error ("Illegitimate gift during boot: " <> show x)


-- SIGINT ----------------------------------------------------------------------

sendSIGINT :: Serf -> IO ()
sendSIGINT serf = do
  getPid (serfProc serf) >>= \case
    Nothing  -> pure ()
    Just pid -> do
      io $ signalProcess sigINT pid


-- Killing the Serf ------------------------------------------------------------


{-|
  Ask the serf to shutdown. If it takes more than 2s, kill it with
  SIGKILL.
-}
stop :: HasLogFunc e => Serf -> RIO e ()
stop serf = do
  race_ niceKill (wait2sec >> forceKill)
 where
  wait2sec = threadDelay 2_000_000

  niceKill = do
    logTrace "Asking serf to shut down"
    io (gracefullyKillSerf serf)
    logTrace "Serf went down when asked."

  forceKill = do
    logTrace "Serf taking too long to go down, kill with fire (SIGTERM)."
    io (forcefullyKillSerf serf)
    logTrace "Serf process killed with SIGTERM."

{-|
  Kill the serf by taking the lock, then asking for it to exit.
-}
gracefullyKillSerf :: Serf -> IO ()
gracefullyKillSerf serf@Serf{..} = do
  -- XX some mechanism for waiting for all effects to be released
  sendShutdownRequest serf 0
  waitForProcess serfProc
  pure ()

{-|
  Kill the serf by sending it a SIGKILL.
-}
forcefullyKillSerf :: Serf -> IO ()
forcefullyKillSerf serf = do
  getPid (serfProc serf) >>= \case
    Nothing  -> pure ()
    Just pid -> do
      io $ signalProcess sigKILL pid
      io $ void $ waitForProcess (serfProc serf)

sendShutdownRequest :: Serf -> Atom -> IO ()
sendShutdownRequest serf exitCode = do
  sendTask serf (TExit ())
  pure ()


-- Running Ship Flow -----------------------------------------------------------

run
  :: Serf
  -> Int
  -> STM EventId
  -> STM RunReq
  -> (Maybe Ev -> STM ())
  -> IO ()
run serf maxBatchSize getLastEvInLog onInput spin = start
 where
  -- New program.
  -- spawn a thread to receive responses from the serf
  -- loop on input:
  --   {
  --   send to serf
  --   pass to recv thread action to take under "lock" expecting response
  --   }
  --   but block if the queue is too big
  -- recv thread
  --   wait to receive from serf by running the expectation action off deque
  --   something about spinners
  --
  -- part 2: gracefully killing waits for in-flights to drain
  --
  -- part 3: spinner
  start :: IO ()
  start = do
    inFlight <- newTBMQueueIO maxBatchSize
    tRecv <- async (recvLoop inFlight)
    sendLoop inFlight `onException` (print "KILLING: recvLoop" >> cancel tRecv)


  sendLoop :: TBMQueue (IO ()) -> IO ()
  sendLoop inFlight = loop
    where
      loop :: IO ()
      loop = atomically onInput >>= \case
        RRWork workErr -> doWork workErr
        RRSave ()      -> doSave
        RRKill ()      -> doKill
        RRPack ()      -> doPack
        RRScry g r k   -> doScry g r k

      doWork :: EvErr -> IO ()
      doWork workErr@(EvErr ev _) = do
        -- pass recv action into queue
        -- send to serf
        now <- Time.now
        let act = do
              work <- recvPoke serf
              onWorkResp workErr work
        atomically $ writeTBMQueue inFlight act
        sendTask serf (TPoke 0 ev)
        loop

      doSave :: IO ()
      doSave = do
        sendTask serf (TSync $ SSave ())
        loop

      doKill :: IO ()
      doKill = do
        atomically $ closeTBMQueue inFlight
        sendTask serf (TExit ())
        pure ()

      doPack :: IO ()
      doPack = do
        atomically $ writeTBMQueue inFlight (recvLive serf)
        sendTask serf (TLive $ LPack ())
        loop

      -- XX support meld

      doScry :: Gang -> ScryReq -> (Maybe (Term, Noun) -> IO ()) -> IO ()
      doScry g r k = do
        atomically $ writeTBMQueue inFlight (recvPeek serf >>= k)
        sendTask serf (TPeek 0 g r)
        loop


  onWorkResp :: EvErr -> Each FX [Goof] -> IO ()
  onWorkResp (EvErr _ err) = \case
    EachYes fx   -> io $ err (RunOkay fx)
    EachNo goofs -> io $ err (RunBail goofs)


  recvLoop :: TBMQueue (IO ()) -> IO ()
  recvLoop inFlight = loop
    where
      loop = do
        atomically (readTBMQueue inFlight) >>= \case
          Nothing  -> print "recvLoop: shutting down"
          Just act -> act >> loop
