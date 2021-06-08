{-|
  Khan: Http Server Driver
-}

module Urbit.Vere.Khan
  ( khan
  , khan'
  )
where

import Urbit.Prelude hiding (Builder)

import Urbit.Arvo                hiding (ServerId, reqUrl)
import Urbit.King.App            ( killKingActionL
                                 , HasKingId(..)
                                 , HasMultiKhanApi(..)
                                 , HasPierEnv(..)
                                 )
import Urbit.King.Config

import Data.List.NonEmpty          (NonEmpty((:|)))
import Data.PEM                    (pemParseBS, pemWriteBS)
import RIO.Prelude                 (decodeUtf8Lenient)
import System.Random               (randomIO)


-- Types -----------------------------------------------------------------------

type HasShipEnv e = (HasLogFunc e, HasNetworkConfig e, HasPierConfig e)

type ReqId = UD

newtype Drv = Drv (MVar (Maybe Sock))

data Sock = Sock
  {
    sLiveReqs  :: TVar LiveReqs
  }

-- Khan Driver -----------------------------------------------------------------

khan'
  :: (HasPierEnv e, HasMultiKhanApi e)
  => Ship
  -> Bool
  -> (Text -> RIO e ())
  -> KingSubsite
  -> RIO e ([Ev], RAcquire e (DriverApi SocketEf))

khan' who isFake stderr sub = do
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
    atomically (leaveMultiKhan multi who)
    io (saKil sLop)
    io (saKil sIns)
    io $ for_ sSec (\sec -> (saKil sec))
    io (removeSocketFile sSocketsFile)

  restart :: Drv -> SocketConf -> RIO e Sock
  restart (Drv var) conf = do
    logInfo "Reconnecting socket"
    let onFatal = runRIO env $ do
          stderr "A socket problem has occurred. Please restart your ship."
          view killKingActionL >>= atomically
    let startAct = connectSocket who isFake conf plan stderr onFatal
    res <- fromEither =<< restartService var startAct kill
    logInfo "Done reconnecting socket"
    pure res

  liveFailed _ = pure ()

  handleEf :: Drv -> SocketEf -> IO ()
  handleEf drv = runRIO env . \case
    HSESetConfig (i, ()) conf -> do
      logInfo (displayShow ("KHAN", "%set-config"))
      Sock {..} <- restart drv conf
      logInfo (displayShow ("KHAN", "%set-config", "Sending %live"))
      atomically $ plan (EvErr (liveEv sServId sSocket) liveFailed)
      logInfo "Write ports file"
      io (write sSocketFile sSocket)
    HSEResponse (i, req, _seq, ()) ev -> do
      logDebug (displayShow ("KHAN", "%response"))
      execRespActs drv who (fromIntegral req) ev
