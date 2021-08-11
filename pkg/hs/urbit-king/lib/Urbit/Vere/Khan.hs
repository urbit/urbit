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
import Data.Either (fromRight)
import qualified Network.Socket.ByteString as BN

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
    sConfig :: SockConf
  }

-- Khan Driver -----------------------------------------------------------------

khan'
  :: (HasPierEnv e)
  => Ship
  -> (Text -> RIO e ())
  -> RIO e ([Ev], RAcquire e (DriverApi SocketEf))

khan' who stderr = do
  ventQ :: TQueue EvErr <- newTQueueIO
  env <- ask

  let (bornEvs, startDriver) =
        khan env who (writeTQueue ventQ) stderr

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
  -> (Text -> RIO e ())
  -> ([Ev], RAcquire e (SocketEf -> IO ()))
khan env who plan stderr = (initialEvents, runSocket)
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
    io (removeSocketFile (scFile sConfig))

  restart :: Drv -> SocketConf -> RIO e Sock
  restart (Drv var) conf = do
    logInfo "Reconnecting socket"
    let onFatal = runRIO env $ do
          stderr "A socket problem has occurred. Please restart your ship."
          view killKingActionL >>= atomically
    let startAct = start plan onFatal
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
      atomically $ plan (EvErr (liveEv sSocketId (scFile sConfig)) liveFailed)
      logInfo (displayShow ("KHAN", "%open-socket", "opening khan socket"))
    SEResponse (i, req, _seq, _) ev -> do
      logDebug (displayShow ("KHAN", "%response"))
      execute drv who (fromIntegral req) ev
    SEError(i, ()) () -> logDebug (displayShow ("KHAN", "%error"))

start :: (HasLogFunc e) => (EvErr -> STM ()) -> IO () -> RIO e Sock
start plan onFatal = do
  logInfo (displayShow ("KHAN", "start"))
  sockId <- io $ SocketId . UV . fromIntegral <$> (randomIO :: IO Word32)
  let evRequest reqId byt = socketEv $
        SocketEvFyrd (sockId, 1, 1, ())
          (SocketReq {
              -- sId = toNoun reqId,
              sRequest = fromRight (toNoun $ socketEv $ SocketEvCancelRequest (sockId, 1, 1, ()) ()) byt
              })

  let onReq :: Word64 -> ByteString -> STM ()
      onReq reqId byt =
        plan $ flip EvErr cancelFailed $ evRequest reqId (cueBS byt)

  let conf@SockConf {..} =
        SockConf
          { scFile = "khan.soc",
            scType = STReq $ SReqApi {sReq = onReq}
          }

  api <- watch onFatal conf
  pure (Sock sockId api conf)

execute :: forall e . HasLogFunc e => Drv -> Ship -> Word64 -> SocketEvent -> RIO e ()
execute (Drv v) who reqId ev = do
  env <- ask
  so <- readMVar v
  case so of
    Nothing -> logDebug $ displayShow ev -- logError "Got a response to a request that does not exist."
    Just Sock {..} ->
      runRIO env $ do
        conn <- atomically (saSock sApi)
        --  TODO The socket is opened on the original request (`onReq`), closed after the work is complete, and another is immediately opened.
        -- (saSock sApi) should save *that* socket, but there are possibly some concurrency issues here.
        -- logDebug $ displayShow (saSock sApi)
        io (BN.sendAll conn (jamBS (toNoun ev)))
