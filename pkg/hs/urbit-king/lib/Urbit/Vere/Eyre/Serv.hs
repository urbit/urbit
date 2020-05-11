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

{-# OPTIONS_GHC -Wno-deprecations #-}

module Urbit.Vere.Eyre.Serv
  ( ServApi(..)
  , TlsConfig(..)
  , MultiTlsConfig
  , ReqApi(..)
  , ServType(..)
  , ServPort(..)
  , ServHost(..)
  , ServConf(..)
  , configCreds
  , serv
  )
where

import Urbit.Prelude hiding (Builder)

import Data.Default       (def)
import Data.List.NonEmpty (NonEmpty((:|)))
import Network.TLS        (Credential, Credentials(..), ServerHooks(..))
import Network.TLS        (credentialLoadX509ChainFromMemory)
import RIO.Prelude        (decodeUtf8Lenient)

import qualified Data.Char                   as C
import qualified Network.Socket              as Net
import qualified Network.Wai                 as W
import qualified Network.Wai.Handler.Warp    as W
import qualified Network.Wai.Handler.WarpTLS as W
import qualified Urbit.Ob                    as Ob
import qualified Urbit.Vere.Eyre.Wai         as E


-- Internal Types --------------------------------------------------------------

data ServApi = ServApi
  { saKil :: STM ()
  , saPor :: STM W.Port
  }

data TlsConfig = TlsConfig
  { tcPrKey :: ByteString
  , tcCerti :: ByteString
  , tcChain :: [ByteString]
  }

type MultiTlsConfig = TVar (Map Ship Credential)

data ReqApi = ReqApi
  { rcReq :: Ship -> Word64 -> E.ReqInfo -> STM ()
  , rcKil :: Ship -> Word64 -> STM ()
  }

data ServType
  = STHttp Ship ReqApi
  | STHttps Ship TlsConfig ReqApi
  | STMultiHttp ReqApi
  | STMultiHttps MultiTlsConfig ReqApi

data ServPort
  = SPAnyPort
  | SPChoices (NonEmpty W.Port)

data ServHost
  = SHLocalhost
  | SHAnyHostOk

data ServConf = ServConf
  { scType :: ServType
  , scHost :: ServHost
  , scPort :: ServPort
  , scRedi :: Maybe W.Port
  }


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
    adr <- Net.inet_addr hos
    Net.bind sok (Net.SockAddrInet Net.defaultPort adr)
    Net.listen sok 1
    port <- Net.socketPort sok
    pure (fromIntegral port, sok)

retry :: HasLogFunc e => RIO e (Either IOError a) -> RIO e a
retry act = act >>= \case
  Right res -> pure res
  Left exn  -> do
    logError (displayShow ("EYRE", "Failed to open ports.", exn))
    logError (displayShow ("EYRE", "Waiting 250ms then trying again."))
    threadDelay 250_000
    retry act

tryOpenChoices
  :: HasLogFunc e
  => String
  -> NonEmpty W.Port
  -> RIO e (Either IOError (W.Port, Net.Socket))
tryOpenChoices hos = go
 where
  go (p :| ps) = do
    logTrace (displayShow ("EYRE", "Trying to open port.", p))
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
  logTrace (displayShow ("EYRE", "Asking the OS for any free port."))
  io (openFreePort hos) >>= \case
    Left exn -> pure (Left exn)
    Right (p,s) -> do
      pure (Right (p,s))

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
    (p, s) <- retry $ case por of
      SPAnyPort    -> tryOpenAny bind
      SPChoices ps -> tryOpenChoices bind ps
    rio $ logTrace $ displayShow ("EYRE", "Opened port.", p)
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

onSniHdr :: TVar (Map Ship Credential) -> Maybe String -> IO Credentials
onSniHdr mtls mHos = do
  ship <- hostShip (encodeUtf8 . pack <$> mHos)
  tabl <- atomically (readTVar mtls)
  tcfg <- lookup ship tabl & maybe (notRunning ship) pure
  pure (Credentials [tcfg])
 where
  notRunning ship = error ("Ship not running: ~" <> show ship)

startServer
  :: HasLogFunc e
  => ServType
  -> ServHost
  -> W.Port
  -> Net.Socket
  -> Maybe W.Port
  -> TVar E.LiveReqs
  -> RIO e ()
startServer typ hos por sok red vLive = do
  envir <- ask

  let host = case hos of
        SHLocalhost -> "127.0.0.1"
        SHAnyHostOk -> "*"

  let opts =
        W.defaultSettings
          & W.setHost host
          & W.setPort (fromIntegral por)
          & W.setTimeout (5 * 60)

  let runAppl who = E.app envir who vLive
      reqShip = hostShip . W.requestHeaderHost

  case typ of
    STHttp who api -> do
      let app = runAppl who (rcReq api who) (rcKil api who)
      io (W.runSettingsSocket opts sok app)

    STHttps who TlsConfig {..} api -> do
      let tls = W.tlsSettingsChainMemory tcCerti tcChain tcPrKey
      let app = runAppl who (rcReq api who) (rcKil api who)
      io (W.runTLSSocket tls opts sok app)

    STMultiHttp api -> do
      let app req resp = do
            who <- reqShip req
            runAppl who (rcReq api who) (rcKil api who) req resp
      io (W.runSettingsSocket opts sok app)

    STMultiHttps mtls api -> do
      let sni = def { onServerNameIndication = onSniHdr mtls }
      let tls = W.defaultTlsSettings { W.tlsServerHooks = sni }
      let app = \req resp -> do
            who <- reqShip req
            runAppl who (rcReq api who) (rcKil api who) req resp

      io (W.runTLSSocket tls opts sok app)


--------------------------------------------------------------------------------

configCreds :: TlsConfig -> Either Text Credential
configCreds TlsConfig {..} =
  credentialLoadX509ChainFromMemory tcCerti tcChain tcPrKey & \case
    Left  str -> Left (pack str)
    Right rs  -> Right rs

serv :: HasLogFunc e => TVar E.LiveReqs -> ServConf -> RIO e ServApi
serv vLive ServConf {..} = do
  kil <- newEmptyTMVarIO
  por <- newEmptyTMVarIO

  void $ async $ do
    tid <- async (runServ por)
    atomically (takeTMVar kil)
    cancel tid

  pure $ ServApi
    { saKil = void (tryPutTMVar kil ())
    , saPor = readTMVar por
    }
 where
  runServ vPort = do
    rwith (forceOpenSocket scHost scPort) $ \(por, sok) -> do
      atomically (putTMVar vPort por)
      startServer scType scHost por sok scRedi vLive
