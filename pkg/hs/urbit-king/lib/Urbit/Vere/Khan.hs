{-|
  Khan: Http Server Driver
-}

module Urbit.Vere.Khan
  ( khan
  , khan'
  )
where

import Urbit.Prelude

import Urbit.Arvo                hiding (ServerId, reqUrl)
import Urbit.King.App            ( killKingActionL
                                 , HasKingId(..)
                                 , HasPierEnv(..)
                                 )

import Urbit.Vere.Pier.Types
import Urbit.Vere.Eyre.Service
import Urbit.Vere.Khan.SocketFile
import Urbit.Vere.Khan.Socket
import System.Random (Random(randomIO))

-- Types -----------------------------------------------------------------------
-- type HasKhanEnv e = (HasLogFunc e, HasPierConfig e)
socketEv :: SocketEv -> Ev
socketEv = EvBlip . BlipEvSocket

bornEv :: KingId -> Ev
bornEv king = socketEv $ SocketEvBorn (king, ()) ()
liveEv :: SocketId ->  FilePath -> Ev
liveEv sId file = socketEv $ SocketEvLive (sId, ()) file

cancelFailed :: WorkError -> IO ()
cancelFailed _ = pure ()

newtype Drv = Drv (MVar (Maybe Sock))

data Sock = Sock
  {
    sSocketId :: SocketId,
    sApi :: SockApi,
    sConfig :: SockConf,
    sFile :: FilePath

  }

-- Khan Driver -----------------------------------------------------------------

khan'
  :: (HasPierEnv e)
  => Ship
  -> Bool
  -> (Text -> RIO e ())
  -> RIO e ([Ev], RAcquire e (DriverApi SocketEf))

khan' who isFake stderr = do
  ventQ :: TQueue EvErr <- newTQueueIO
  env <- ask

  let (bornEvs, startDriver) =
        khan env who (writeTQueue ventQ) isFake stderr

  let runDriver = do
        diOnEffect <- startDriver
        let diEventSource = fmap RRWork <$> tryReadTQueue ventQ
        pure (DriverApi {..})

  pure (bornEvs, runDriver)

{-|
  Khan -- IO Driver
-}
khan
  :: forall e
   . (HasPierEnv e)
  => e
  -> Ship
  -> (EvErr -> STM ())
  -> Bool
  -> (Text -> RIO e ())
  -> ([Ev], RAcquire e (SocketEf -> IO ()))
khan env who plan isFake stderr = (initialEvents, runSocket)
 where
  king = fromIntegral (env ^. kingIdL)
  initialEvents :: [Ev]
  initialEvents = [bornEv king]

  runSocket :: RAcquire e (SocketEf -> IO ())
  runSocket = handleEf <$> mkRAcquire
    (Drv <$> newMVar Nothing)
    (\(Drv v) -> stopService v kill >>= fromEither)

  kill :: HasLogFunc e => Sock -> RIO e ()
  kill Sock{..} = do
    io (saKil sApi)
    io (removeSocketFile sFile)

  restart :: Drv -> SocketConf -> RIO e Sock
  restart (Drv var) conf = do
    logInfo "Reconnecting socket"
    let onFatal = runRIO env $ do
          stderr "A socket problem has occurred. Please restart your ship."
          view killKingActionL >>= atomically
    let startAct = startServ plan onFatal
    res <- fromEither =<< restartService var startAct kill
    logInfo "Done reconnecting socket"
    pure res

  liveFailed _ = pure ()

  handleEf :: Drv -> SocketEf -> IO ()
  handleEf drv = runRIO env . \case
    SESetConfig (i, ()) conf -> do
      logInfo (displayShow ("KHAN", "%set-config"))
      Sock {..} <- restart drv conf
      logInfo (displayShow ("KHAN", "%set-config", "Sending %live"))
      atomically $ plan (EvErr (liveEv sSocketId sFile) liveFailed)

      logInfo (displayShow ("KHAN", "%open-socket", "opening khan socket"))
    SEResponse (i, req, _seq, ()) ev -> do
      logDebug (displayShow ("KHAN", "%response"))
      execRespActs drv who (fromIntegral req) ev
    SEError(i, ()) () -> logDebug (displayShow ("KHAN", "%error"))



startServ :: HasLogFunc e => (EvErr -> STM ()) -> IO () -> RIO e Sock
startServ plan onFatal = do
    logInfo (displayShow ("KHAN", "startServ"))
    sockId <- io $ SocketId . UV . fromIntegral <$> (randomIO :: IO Word32)
    let onReq :: Ship -> Word64 -> STM ()
        onReq _ship reqId =
          plan $ (flip EvErr cancelFailed) $ socketEv $ SocketEvRequest (0, 1, 1, ()) (SocketReq {sFile = "khan.soc", sRequest = toNoun reqId})
    let conf@SockConf{..} = SockConf {
            scFilePath = "khan.soc"
          , scType = STReq $ SReqApi { sReq = onReq }
          }

    api <- serv onFatal conf
    pure (Sock sockId api conf scFilePath)

-- runClient socketFile client = do
--     let addr = SockAddrUnix $ socketFile
--     E.bracket (open addr) close client
--   where
--     open addr = E.bracketOnError (socket AF_UNIX Stream 0) close $ \sock -> do
--         connect sock $ addr
--         return sock
-- runServer = forever $
--   E.bracketOnError (accept ss) (close . fst) $
--   \(conn, _peer) -> void $ forkFinally (NBS.sendAll conn "pong") (const $ gracefulClose conn 500)
execRespActs :: HasLogFunc e => Drv -> Ship -> Word64 -> SocketEvent -> RIO e ()
execRespActs (Drv v) who reqId ev = readMVar v >>= \case
  Nothing -> logError "Got a response to a request that does not exist."
  Just sv -> do
    logDebug $ displayShow ev
    for_ (parseSocketEvent ev) $ \act -> do
      logDebug (displayShow ("KHAN", "%exec"))

parseSocketEvent :: SocketEvent -> [Noun]
parseSocketEvent = \case
  SocketStart n  -> [n]
  SocketContinue n -> [n]
  SocketCancel ()       -> [toNoun 'o']
