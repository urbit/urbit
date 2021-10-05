
module Urbit.Vere.Khan.Socket
  ( SockApi(..)
  , watch
  , openSocket
  , tryConnect
  , recvOnSocket
  , trySend
  , retry
  , SockType(..)
  , SReqApi(..)
  , SockConf(..)
  , SocketResponse(..)
  )
where

import Urbit.Prelude hiding (Builder)

import Network.Socket as Net

import qualified Network.Socket.ByteString.Lazy as LN
import qualified Network.Socket.ByteString as BN
import qualified Data.ByteString.Lazy as L

-- Internal Types --------------------------------------------------------------
-- TODO Maybe refactor naming? Khan instead of Socket.
data SocketExn
  = BadNoun ByteString
  | MissingSocket
 deriving Show

data SockApi = SockApi
  { saKil :: IO ()
  , saAddr :: STM SockAddr
  , saSock :: STM Socket
  }

data SockConf = SockConf {
  scFile :: FilePath,
  scType :: SockType
  }
  deriving (Show)

data SockType = STGeneric
    | STReq SReqApi
data SocketResponse = SocketResponse {scResp :: ByteString}
data SReqApi = SReqApi
  { sReq :: Word64 -> ByteString -> STM ()
  -- , sKil :: Ship -> Word64 -> STM ()
  }
instance Show SockType where
  show = \case
    STReq _      -> "STReq"
    STGeneric -> "STGeneric"
instance Exception SocketExn where
bindListen :: Net.Socket -> FilePath -> IO SockAddr
bindListen sok file = do
  let adr = SockAddrUnix $ file
  Net.bind sok $ adr
  Net.listen sok 5
  pure $ adr

unixSocket :: IO (Either IOError Net.Socket)
unixSocket =
  tryIOError (Net.socket Net.AF_UNIX Net.Stream Net.defaultProtocol)


trySend :: Net.Socket -> SockAddr ->  ByteString -> RIO e (Either IOError ())
trySend sok adr bs = ssend
  where
    ssend = io (tryIOError $ BN.sendAll sok bs) <&> \case
      Left exn -> Left exn
      Right _ -> Right ()

tryConnect :: Net.Socket -> SockAddr -> RIO e (Either IOError ())
tryConnect sok adr = io (tryIOError $ Net.connect sok adr)
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
openSocket
  :: forall e
   . HasLogFunc e
  => FilePath
  -> RAcquire e (SockAddr, Net.Socket)
openSocket socketFile = mkRAcquire opn kil
 where
  kil (_bs, sok) = do
    logDbg ctx ("in openSocket", "closing socket")
    io $ Net.close sok

  opn = do
    (a, s) <- retry $ io $ tryOpen socketFile
    pure (a, s)
  ctx = ["KHAN", "WATCH", "openSocket"]
recvOnSocket
  :: forall e
   . HasLogFunc e
  => Socket
  -> RAcquire e (L.ByteString, Net.Socket)
recvOnSocket s = mkRAcquire rcv kil
 where
  kil (_bs, sok)= do
    logDbg ctx ("in recv", "closing socket")
    io $ Net.close sok

  rcv = do
    logDbg ctx ("in recv", "unix socket")
    -- TODO how should we handle size limits?
    res <- io $ LN.recv s 400000
    logDbg ctx ("received", res)
    pure (res, s)
  ctx = ["KHAN", "WATCH", "recvOnSocket"]
watch :: HasLogFunc e
         => IO () -> SockConf -> RIO e SockApi
watch onFatal SockConf {..} = do
  logInfo (displayShow ("KHAN", "WATCH", "Running Khan"))
  adr <- newEmptyTMVarIO
  sok <- newEmptyTMVarIO
  tid <- async (runWatch adr sok)
  pure $ SockApi { saKil = cancel tid,
                   saAddr = readTMVar adr,
                   saSock = readTMVar sok
                 }
 where
  runWatch vAddr vSock = do
    logInfo (displayShow ("KHAN", "WATCH", "runWatch"))

    rwith (openSocket scFile) $ \((adr, sok)) -> do
      atomically (putTMVar vSock sok)
      atomically (putTMVar vAddr adr)
      io $ setSocketOption sok ReuseAddr 1
      watchSocket scType sok onFatal

watchSocket
  :: HasLogFunc e
  => SockType
  -> Net.Socket
  -> IO ()
  -> RIO e ()
watchSocket typ sok onFatal = do
  envir <- ask

  let handler r e = do
        when (isFatal e) $ do
          runRIO envir $ logError $ display $ msg r e
          onFatal
      isFatal e
        | (BadNoun byt) <- e
        = False
        | MissingSocket <- e
        = True
        | otherwise = False

      msg r e = case r of
        Just r  -> "khan: failed request from " <> (tshow $ sok)
                <> " for " <> (tshow $ r) <> ": " <> tshow e
        Nothing -> "khan: server exception: " <> tshow e

  case typ of
    STReq SReqApi {..} -> do
      runSocket sok sReq handler
    STGeneric -> do
      runGeneric sok
  pure ()
  where
    runSocket sok onReq handler = do
      env <- ask
      io $ forever $ do
        runRIO env $ logDebug (displayShow ("KHAN", "SOCKET", "accepting connections on socket" <> (show sok)))
        (conn, adr) <- accept sok
        runRIO env $ do
          rwith (recvOnSocket conn) $ \(dat, s_) -> do
            logInfo (displayShow ("KHAN", "SOCKET", "message on socket"))
            let byt = toStrict dat
            let cued = (cueBS $ byt)
            req <- do
                  case cued of
                    Left exn -> io $ handler (Just byt) (BadNoun byt) >> pure byt
                    Right n -> pure $ jamBS n
            runRIO env $ logDbg ctx ("runSocket", (show $ req))
              -- TODO Track live socket requests
            atomically $ onReq 1 req

    runGeneric sok = do
      env <- ask
      io $ forever $ do
        (conn, SockAddrUnix fil) <- accept sok
        runRIO env $ do
          logInfo (displayShow ("KHAN", "SOCKET", "message on socket"))

          rwith (recvOnSocket conn) $ \(dat, s_) -> do
            let n = (cueBS $ toStrict dat)
            let res = either (\_ -> toStrict dat) jamBS n

            runRIO env $ logInfo (displayShow ("KHAN", "SOCKET", "msg: " <> (show $ n)))
            io $ (BN.sendAll conn res)
    ctx = ["KHAN", "SOCKET", "run"]
