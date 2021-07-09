
module Urbit.Vere.Khan.Socket
  ( SockApi(..)
  , serv
  , SockType(..)
  , SReqApi(..)
  , SockConf(..)
  )
where

import Urbit.Prelude hiding (Builder)

import Network.Socket as Net
import qualified Control.Exception as E
import qualified Network.Socket.ByteString as NBS


-- Internal Types --------------------------------------------------------------

data SockApi = SockApi
  { saKil :: IO ()
  , saFile :: STM FilePath
  }

data SockConf = SockConf { scFilePath :: FilePath, scType :: SockType }
  deriving (Show)

data SockType = STGeneric
    | STReq SReqApi

data SReqApi = SReqApi
  { sReq :: Ship -> Word64 -> STM ()
  -- , sKil :: Ship -> Word64 -> STM ()
  }
instance Show SockType where
  show = \case
    STReq _      -> "STReq"
    STGeneric -> "STGeneric"

bindListen :: Net.Socket -> FilePath -> IO SockAddr
bindListen sok file = do
  Net.bind sok . SockAddrUnix $ file
  Net.listen sok 1
  pure $ SockAddrUnix $ file

unixSocket :: IO (Either IOError Net.Socket)
unixSocket =
  tryIOError (Net.socket Net.AF_UNIX Net.Stream Net.defaultProtocol)

tryOpen :: FilePath -> IO (Either IOError (SockAddr, Net.Socket))
tryOpen socketFile =
  unixSocket >>= \case
    Left  exn -> pure (Left exn)
    Right sok -> tryIOError (bindListen sok socketFile) >>= \case
      Left  exn -> Net.close sok $> Left exn
      Right adr  -> pure (Right (adr, sok))

retry :: HasLogFunc e => RIO e (Either IOError a) -> RIO e a
retry act = act >>= \case
  Right res -> pure res
  Left  exn -> do
    logDbg ctx ("Failed to open socket. Waiting 5s, then trying again.", exn)
    threadDelay 5_000_000
    retry act
 where
  ctx = ["KHAN", "SERV", "retry"]
logDbg :: (HasLogFunc e, Show a) => [Text] -> a -> RIO e ()
logDbg ctx msg = logInfo (prefix <> suffix)
 where
  prefix = display (concat $ fmap (<> ": ") ctx)
  suffix = displayShow msg
forceOpenSocket
  :: forall e
   . HasLogFunc e
  => FilePath
  -> RAcquire e (SockAddr, Net.Socket)
forceOpenSocket socketFile = mkRAcquire opn kil
 where
  kil = io . Net.close . snd

  opn = do
    (a, s) <- retry $ io $ tryOpen socketFile
    pure (a, s)
recvOnSocket
  :: forall e
   . HasLogFunc e
  => Socket
  -> RAcquire e (ByteString, Net.Socket)
recvOnSocket s = mkRAcquire rcv kil
 where
  kil = io . Net.close . snd

  rcv = do
    res <- io $ NBS.recv s 400000
    pure (res, s)

startServer
  :: HasLogFunc e
  => Net.Socket
  -> IO ()
  -> RIO e ()
startServer sok onFatal = do
  envir <- ask

  -- let handler r e = do
  --       when (isFatal e) $ do
  --         runRIO envir $ logError $ display $ msg r e
  --         onFatal
  --     isFatal e
  --       | Just (IOError {ioe_type = ResourceExhausted}) <- fromException e
  --       = True
  --       | otherwise = False

  --     msg r e = case r of
  --       Just r  -> "khan: failed request from " <> (tshow $ W.remoteHost r)
  --               <> " for " <> (tshow $ r) <> ": " <> tshow e
  --       Nothing -> "khan: server exception: " <> tshow e

  runUnixSocket sok
  pure ()


--------------------------------------------------------------------------------

realServ :: HasLogFunc e
         => IO () -> SockConf -> RIO e SockApi
realServ onFatal conf@SockConf {..} = do
  logInfo (displayShow ("KHAN", "SERV", "Running Real Server"))
  fil <- newEmptyTMVarIO
  tid <- async (runServ fil)
  pure $ SockApi { saKil = cancel tid,
                   saFile = readTMVar fil
                 }
 where
  runServ vFile = do
    logInfo (displayShow ("KHAN", "SERV", "runServ"))
    rwith (forceOpenSocket scFilePath) $ \(sok) -> do
      atomically (putTMVar vFile scFilePath)
      startServer (snd sok) onFatal

serv :: HasLogFunc e =>  IO () -> SockConf -> RIO e SockApi
serv onFatal conf = realServ onFatal conf

runUnixSocket :: (HasLogFunc e) => Net.Socket -> RIO e ByteString
runUnixSocket sok = do
  env <- ask
  io $ forever $ E.bracketOnError (accept sok) (close . fst) $
    \(conn, SockAddrUnix fil) -> runRIO env $ do
      logInfo (displayShow ("KHAN", "SOCKET", "message on socket"))
      rwith (recvOnSocket sok) $ \(dat, s_) -> do
        runRIO env $ logInfo (displayShow ("KHAN", "SOCKET", "msg: " <> tshow dat))
        io $ (NBS.sendAll conn dat)
      -- byt <- io $ toStrict <$> (NBS.recv sok 4000000)
      -- dat <- cueBSExn byt >>= fromNounExn

      -- forkFinally  (atomically $ socketFinally conn)
      -- where
      --   socketFinally conn = \case
      --     Right rr -> pure rr
      --     Left err -> withRIOThread $ do
      --       logError (displayShow ("KHAN", "SOCKET", "error on socket: " <> tshow err))
      --       gracefulClose conn 5000

-- withRIOThread :: (HasLogFunc e) => RIO e a -> RIO e (Async a)
-- withRIOThread act = do
--     env <- ask
--     io $ async $ runRIO env $ act
