{-|
  Runs a single HTTP (or HTTPS) server for the eyre driver.

  A server is given:

  - A port, or a range or ports.
  - Opens a socket on one of those ports.
  - If this fails, try again repeatedly.
  - Once a socket is opened, runs an HTTP server on the specified port.
  - Once the server is up, calls a callback with the port that was opened.
  - Once we have chosen a port, we commit to that port (ignoring the
    original range).
  - If the socket ever goes down, keep trying to reopen that port forever.
  - When the server is shutdown, make sure the socket is closed.

  TODO How to detect socket closed during server run?
-}

module Urbit.Vere.Eyre.Serv
  ( ServApi(..)
  , TlsConfig(..)
  , MultiTlsConfig(..)
  , ReqApi(..)
  , ServType(..)
  , ServPort(..)
  , ServHost(..)
  , ServConf(..)
  , configCreds
  , serv
  , fakeServ
  )
where

import Urbit.Prelude hiding (Builder)

import Data.Default                (def)
import Data.List.NonEmpty          (NonEmpty((:|)))
import GHC.IO.Exception            (IOException(..), IOErrorType(..))
import Network.TLS                 ( Credential
                                   , Credentials(..)
                                   , ServerHooks(..)
                                   )
import Network.TLS                 (credentialLoadX509ChainFromMemory)
import RIO.Prelude                 (decodeUtf8Lenient)
import Urbit.Vere.Eyre.KingSubsite (KingSubsite)

import qualified Control.Monad.STM           as STM
import qualified Data.Char                   as C
import qualified Network.Socket              as Net
import qualified Network.Wai                 as W
import qualified Network.Wai.Handler.Warp    as W
import qualified Network.Wai.Handler.WarpTLS as W
import qualified Urbit.Ob                    as Ob
import qualified Urbit.Vere.Eyre.Wai         as E


-- Internal Types --------------------------------------------------------------

data ServApi = ServApi
  { saKil :: IO ()
  , saPor :: STM W.Port
  }

data TlsConfig = TlsConfig
  { tcPrKey :: ByteString
  , tcCerti :: ByteString
  , tcChain :: [ByteString]
  }
 deriving (Show)

newtype MultiTlsConfig = MTC (TVar (Map Ship (TlsConfig, Credential)))

data ReqApi = ReqApi
  { rcReq :: Ship -> Word64 -> E.ReqInfo -> STM ()
  , rcKil :: Ship -> Word64 -> STM ()
  }

data ServType
  = STHttp Ship KingSubsite ReqApi
  | STHttps Ship TlsConfig KingSubsite ReqApi
  | STMultiHttp (Ship -> STM KingSubsite) ReqApi
  | STMultiHttps MultiTlsConfig (Ship -> STM KingSubsite) ReqApi

instance Show ServType where
  show = \case
    STHttp  who _ _      -> "STHttp "  <> show who
    STHttps who tls _ _  -> "STHttps " <> show who <> " " <> show tls
    STMultiHttp _ _      -> "STMultiHttp"
    STMultiHttps tls _ _ -> "STMultiHttps"

data ServPort
  = SPAnyPort
  | SPChoices (NonEmpty W.Port)
 deriving (Show)

data ServHost
  = SHLocalhost
  | SHAnyHostOk
 deriving (Show)

data ServConf = ServConf
  { scType :: ServType
  , scHost :: ServHost
  , scPort :: ServPort
  , scRedi :: Maybe W.Port
  , scFake :: Bool
  }
 deriving (Show)


-- Opening Sockets -------------------------------------------------------------

getBindAddr :: String -> W.Port -> IO Net.SockAddr
getBindAddr hos por =
  Net.getAddrInfo Nothing (Just hos) (Just (show por)) >>= \case
    []    -> error "this should never happen."
    x : _ -> pure (Net.addrAddress x)

bindListenPort :: String -> W.Port -> Net.Socket -> IO Net.PortNumber
bindListenPort hos por sok = do
  Net.bind sok =<< getBindAddr hos por
  Net.listen sok 1
  Net.socketPort sok

tcpSocket :: IO (Either IOError Net.Socket)
tcpSocket =
  tryIOError (Net.socket Net.AF_INET Net.Stream Net.defaultProtocol)

tryOpen :: String -> W.Port -> IO (Either IOError (W.Port, Net.Socket))
tryOpen hos por =
  tcpSocket >>= \case
    Left  exn -> pure (Left exn)
    Right sok -> tryIOError (bindListenPort hos por sok) >>= \case
      Left  exn -> Net.close sok $> Left exn
      Right por -> pure (Right (fromIntegral por, sok))

openFreePort :: String -> IO (Either IOError (W.Port, Net.Socket))
openFreePort hos = do
  tcpSocket >>= \case
    Left exn -> pure (Left exn)
    Right sok -> tryIOError (doBind sok) >>= \case
      Left exn -> Net.close sok $> Left exn
      Right ps -> pure (Right ps)
 where
  doBind sok = do
    adr <-
      Net.getAddrInfo Nothing (Just hos) Nothing >>= \case
        []     -> error ("unable to determine numeric hostname from " ++ hos)
        ip : _ -> pure (Net.addrAddress ip)
 
    Net.bind sok adr
    Net.listen sok 1
    port <- Net.socketPort sok
    pure (fromIntegral port, sok)

retry :: HasLogFunc e => RIO e (Either IOError a) -> RIO e a
retry act = act >>= \case
  Right res -> pure res
  Left  exn -> do
    logDbg ctx ("Failed to open ports. Waiting 5s, then trying again.", exn)
    threadDelay 5_000_000
    retry act
 where
  ctx = ["EYRE", "SERV", "retry"]

tryOpenChoices
  :: HasLogFunc e
  => String
  -> NonEmpty W.Port
  -> RIO e (Either IOError (W.Port, Net.Socket))
tryOpenChoices hos = go
 where
  go (p :| ps) = do
    logInfo (displayShow ("EYRE", "Trying to open port.", p))
    io (tryOpen hos p) >>= \case
      Left err -> do
        logError (displayShow ("EYRE", "Failed to open port.", p))
        case ps of
          []     -> pure (Left err)
          q : qs -> go (q :| qs)
      Right (p, s) -> do
        pure (Right (p, s))

tryOpenAny
  :: HasLogFunc e => String -> RIO e (Either IOError (W.Port, Net.Socket))
tryOpenAny hos = do
  let ctx = ["EYRE", "SERV", "tryOpenAny"]
  logDbg ctx "Asking the OS for any free port."
  io (openFreePort hos) >>= \case
    Left  exn    -> pure (Left exn)
    Right (p, s) -> do
      pure (Right (p, s))

logDbg :: (HasLogFunc e, Show a) => [Text] -> a -> RIO e ()
logDbg ctx msg = logInfo (prefix <> suffix)
 where
  prefix = display (concat $ fmap (<> ": ") ctx)
  suffix = displayShow msg

forceOpenSocket
  :: forall e
   . HasLogFunc e
  => ServHost
  -> ServPort
  -> RAcquire e (W.Port, Net.Socket)
forceOpenSocket hos por = mkRAcquire opn kil
 where
  kil = io . Net.close . snd

  opn = do
    let ctx = ["EYRE", "SERV", "forceOpenSocket"]
    logDbg ctx (hos, por)
    (p, s) <- retry $ case por of
      SPAnyPort    -> tryOpenAny bind
      SPChoices ps -> tryOpenChoices bind ps
    logDbg ctx ("Opened port.", p)
    pure (p, s)

  bind = case hos of
    SHLocalhost -> "127.0.0.1"
    SHAnyHostOk -> "0.0.0.0"


-- Starting WAI ----------------------------------------------------------------

hostShip :: Maybe ByteString -> IO Ship
hostShip Nothing   = error "Request must contain HOST header."
hostShip (Just bs) = byteShip (hedLabel bs) & \case
  Left  err -> error ("Bad host prefix. Must be a ship name: " <> unpack err)
  Right sp  -> pure sp
 where
  byteShip = fmap (fromIntegral . Ob.fromPatp) . bytePatp
  bytePatp = Ob.parsePatp . decodeUtf8Lenient
  hedLabel = fst . break (== fromIntegral (C.ord '.'))

onSniHdr
  :: HasLogFunc e => e -> MultiTlsConfig -> Maybe String -> IO Credentials
onSniHdr env (MTC mtls) mHos = do
  tabl <- atomically (readTVar mtls)
  runRIO env $ logDbg ctx (tabl, mHos)
  ship <- hostShip (encodeUtf8 . pack <$> mHos)
  runRIO env $ logDbg ctx ship
  tcfg <- lookup ship tabl & maybe (notRunning ship) (pure . snd)
  runRIO env $ logDbg ctx tcfg
  pure (Credentials [tcfg])
 where
  notRunning ship = error ("Ship not running: ~" <> show ship)
  ctx = ["EYRE", "HTTPS", "SNI"]

startServer
  :: HasLogFunc e
  => ServType
  -> ServHost
  -> W.Port
  -> Net.Socket
  -> Maybe W.Port
  -> TVar E.LiveReqs
  -> IO ()
  -> RIO e ()
startServer typ hos por sok red vLive onFatal = do
  envir <- ask

  let host = case hos of
        SHLocalhost -> "127.0.0.1"
        SHAnyHostOk -> "*"

  let handler r e = do
        when (isFatal e) $ do
          runRIO envir $ logError $ display $ msg r e
          onFatal
        when (W.defaultShouldDisplayException e) $ do
          runRIO envir $ logWarn $ display $ msg r e

      isFatal e
        | Just (IOError {ioe_type = ResourceExhausted}) <- fromException e
        = True
        | otherwise = False

      msg r e = case r of
        Just r  -> "eyre: failed request from " <> (tshow $ W.remoteHost r)
                <> " for " <> (tshow $ W.rawPathInfo r) <> ": " <> tshow e
        Nothing -> "eyre: server exception: " <> tshow e

  let opts =
        W.defaultSettings
          & W.setHost host
          & W.setPort (fromIntegral por)
          & W.setTimeout 30
          & W.setOnException handler

  -- TODO build Eyre.Site.app in pier, thread through here
  let runAppl who = E.app envir who vLive
      reqShip = hostShip . W.requestHeaderHost

  case typ of
    STHttp who sub api -> do
      let app = runAppl who (rcReq api who) (rcKil api who) sub
      io (W.runSettingsSocket opts sok app)

    STHttps who TlsConfig {..} sub api -> do
      let tls = W.tlsSettingsChainMemory tcCerti tcChain tcPrKey
      let app = runAppl who (rcReq api who) (rcKil api who) sub
      io (W.runTLSSocket tls opts sok app)

    STMultiHttp fub api -> do
      let app req resp = do
            who <- reqShip req
            sub <- atomically $ fub who
            runAppl who (rcReq api who) (rcKil api who) sub req resp
      io (W.runSettingsSocket opts sok app)

    STMultiHttps mtls fub api -> do
      TlsConfig {..} <- atomically (getFirstTlsConfig mtls)

      let sni = def { onServerNameIndication = onSniHdr envir mtls }

      let tlsSing = (W.tlsSettingsChainMemory tcCerti tcChain tcPrKey)
      let tlsMany = tlsSing { W.tlsServerHooks = sni }

      let ctx = ["EYRE", "HTTPS", "REQ"]

      let
        app = \req resp -> do
          runRIO envir $ logDbg ctx "Got request"
          who <- reqShip req
          runRIO envir $ logDbg ctx ("Parsed HOST", who)
          sub <- atomically $ fub who
          runAppl who (rcReq api who) (rcKil api who) sub req resp

      io (W.runTLSSocket tlsMany opts sok app)


--------------------------------------------------------------------------------

configCreds :: TlsConfig -> Either Text Credential
configCreds TlsConfig {..} =
  credentialLoadX509ChainFromMemory tcCerti tcChain tcPrKey & \case
    Left  str -> Left (pack str)
    Right rs  -> Right rs

fakeServ :: HasLogFunc e => ServConf -> RIO e ServApi
fakeServ conf = do
  let por = fakePort (scPort conf)
  logInfo (displayShow ("EYRE", "SERV", "Running Fake Server", por))
  pure $ ServApi
    { saKil = pure ()
    , saPor = pure por
    }
 where
  fakePort :: ServPort -> W.Port
  fakePort SPAnyPort            = 55555
  fakePort (SPChoices (x :| _)) = x

getFirstTlsConfig :: MultiTlsConfig -> STM TlsConfig
getFirstTlsConfig (MTC var) = do
  map <- readTVar var
  case toList map of
    []  -> STM.retry
    x:_ -> pure (fst x)

realServ :: HasLogFunc e
         => TVar E.LiveReqs -> IO () -> ServConf -> RIO e ServApi
realServ vLive onFatal conf@ServConf {..} = do
  logInfo (displayShow ("EYRE", "SERV", "Running Real Server"))
  por <- newEmptyTMVarIO

  tid <- async (runServ por)

  pure $ ServApi
    { saKil = cancel tid
    , saPor = readTMVar por
    }
 where
  runServ vPort = do
    logInfo (displayShow ("EYRE", "SERV", "runServ"))
    rwith (forceOpenSocket scHost scPort) $ \(por, sok) -> do
      atomically (putTMVar vPort por)
      startServer scType scHost por sok scRedi vLive onFatal

serv :: HasLogFunc e => TVar E.LiveReqs -> IO () -> ServConf -> RIO e ServApi
serv vLive onFatal conf = do
  if scFake conf
    then fakeServ conf
    else realServ vLive onFatal conf
