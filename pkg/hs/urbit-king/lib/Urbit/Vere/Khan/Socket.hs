
module Urbit.Vere.Khan.Socket
  ( SockApi(..)
  , serv
  , openSocket
  , recvOnSocket
  , SockType(..)
  , SReqApi(..)
  , SockConf(..)
  , SocketResponse(..)
  )
where

import Urbit.Prelude hiding (Builder)

import Network.Socket as Net
-- import qualified Network.Socket.ByteString as NBS
import qualified Network.Socket.ByteString.Lazy as LN
import qualified Network.Socket.ByteString as BN
import qualified Data.ByteString.Lazy as L
-- import Urbit.Vere.Khan.Protocol
-- import Data.Binary.Get (pushEndOfInput, pushChunk, runGetIncremental, Decoder(..))


-- Internal Types --------------------------------------------------------------
data SocketExn
  = BadNoun ByteString
  | MissingSocket
 deriving Show

data SockApi = SockApi
  { saKil :: IO ()
  , saFile :: STM FilePath
  , saAddr :: STM SockAddr
  , saSock :: STM Socket
  }

data SockConf = SockConf { scFilePath :: FilePath, scType :: SockType }
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
openSocket
  :: forall e
   . HasLogFunc e
  => FilePath
  -> RAcquire e (SockAddr, Net.Socket)
openSocket socketFile = mkRAcquire opn kil
 where
  kil = io . Net.close . snd

  opn = do
    (a, s) <- retry $ io $ tryOpen socketFile
    pure (a, s)
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
    res <- io $ LN.recv s 400000
    logDbg ctx ("received", res)
    pure (res, s)
  ctx = ["KHAN", "SERV", "recvOnSocket"]
serv :: HasLogFunc e
         => IO () -> SockConf -> RIO e SockApi
serv onFatal conf@SockConf {..} = do
  logInfo (displayShow ("KHAN", "SERV", "Running Khan Server"))
  fil <- newEmptyTMVarIO
  adr <- newEmptyTMVarIO
  sok <- newEmptyTMVarIO
  tid <- async (runServ fil adr sok)
  pure $ SockApi { saKil = cancel tid,
                   saFile = readTMVar fil,
                   saAddr = readTMVar adr,
                   saSock = readTMVar sok
                 }
 where
  runServ vFile vAddr vSock = do
    logInfo (displayShow ("KHAN", "SERV", "runServ"))
    rwith (openSocket scFilePath) $ \(sok) -> do
      atomically (putTMVar vFile scFilePath)

      startServer scType vAddr vSock (snd sok) onFatal

startServer
  :: HasLogFunc e
  => SockType
  -> TMVar SockAddr
  -> TMVar Socket
  -> Net.Socket
  -> IO ()
  -> RIO e ()
startServer typ vAddr vSock sok onFatal = do
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
        (conn, addr) <- accept sok
        runRIO env $ do
          atomically (putTMVar vAddr addr)
          atomically (putTMVar vSock conn)
          logInfo (displayShow ("KHAN", "SOCKET", "message on socket"))
          rwith (recvOnSocket conn) $ \(dat, s_) -> do
            let byt = toStrict dat
            let cued = (cueBS $ byt)
            req <- do
                  case cued of
                    Left exn -> io $ handler (Just byt) (BadNoun byt) >> pure byt
                    Right n -> pure $ jamBS n
            runRIO env $ logDbg ctx ("runSocket", (show $ req))
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
