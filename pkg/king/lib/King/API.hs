module King.API where

import UrbitPrelude
import Data.Aeson
import RIO.Directory

import Arvo           (Belt)
import Network.Socket (Socket)
import Prelude        (read)
import Vere.LockFile  (lockFile)

import qualified Network.HTTP.Types             as H
import qualified Network.Wai                    as W
import qualified Network.Wai.Handler.Warp       as W
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets             as WS
import qualified Vere.NounServ                  as NounServ
import qualified Vere.Term.API                  as Term


--------------------------------------------------------------------------------

data King = King
    { kServer :: Async ()
    , kFleet  :: FleetCtl
    }

type TermConn = NounServ.Conn Belt [Term.Ev]

data ShipCtl = ShipCtl
  { scTerm :: TermConn -> STM ()
  }

type FleetCtl = TVar (Map Ship ShipCtl)

data ShipStatus = Halted | Booting | Booted | Running | LandscapeUp
  deriving (Generic, ToJSON, FromJSON)

data KingStatus = Starting | Started
  deriving (Generic, ToJSON, FromJSON)

data StatusResp = StatusResp
    { king  :: KingStatus
    , ships :: Map Text ShipStatus
    }
  deriving (Generic, ToJSON, FromJSON)

data BadShip = BadShip Text Text
  deriving (Show, Exception)


--------------------------------------------------------------------------------

portsFilePath :: MonadIO m => m (FilePath, FilePath)
portsFilePath = do
    hom <- getHomeDirectory
    dir <- pure (hom </> ".urbit")
    fil <- pure (dir </> ".http.ports")
    pure (dir, fil)

portsFile :: Word -> RAcquire e (FilePath, FilePath)
portsFile por = mkRAcquire mkFile (removeFile . snd)
  where
    mkFile = do
        (dir, fil) <- portsFilePath
        createDirectoryIfMissing True dir
        writeFile fil (encodeUtf8 $ tshow por)
        pure (dir, fil)

readPortsFile :: RIO e (Maybe Word)
readPortsFile = do
    (_, fil) <- portsFilePath
    bs <- readFile fil
    evaluate (readMay $ unpack $ decodeUtf8 bs)

kingAPI :: HasLogFunc e => RAcquire e King
kingAPI = do
    api        <- newTVarIO mempty
    (por, soc) <- io $ W.openFreePort
    (dir, fil) <- portsFile (fromIntegral por)
    lockFile dir
    mkRAcquire (startKing (por, soc) api) stopKing

stopKing :: King -> RIO e ()
stopKing = cancel . kServer

startKing :: HasLogFunc e => (Int, Socket) -> FleetCtl -> RIO e King
startKing (port, sock) api = do
    let opts = W.defaultSettings & W.setPort port

    env <- ask

    tid <- async $ io $ W.runSettingsSocket opts sock $ app env api

    pure (King tid api)

stubStatus :: StatusResp
stubStatus = StatusResp Started $ mapFromList [("zod", Running)]

serveTerminal :: HasLogFunc e
              => e -> FleetCtl -> Ship -> Word -> W.Application
serveTerminal env api ship word =
    WS.websocketsOr WS.defaultConnectionOptions wsApp fallback
  where
    fallback req respond =
        respond $ W.responseLBS H.status500 [] "This endpoint uses websockets"

    wsApp pen =
        atomically (lookup ship <$> readTVar api) >>= \case
            Nothing -> WS.rejectRequest pen "Ship not running"
            Just sp -> do
                wsc <- io $ WS.acceptRequest pen
                inp <- io $ newTBMChanIO 5
                out <- io $ newTBMChanIO 5
                atomically $ scTerm sp (NounServ.mkConn inp out)
                runRIO env $
                    NounServ.wsConn "NOUNSERV (wsServ) " inp out wsc

readShip :: Text -> IO Ship
readShip t = parsePat t & \case
     Left err -> throwIO (BadShip t err)
     Right sp -> pure sp

app :: HasLogFunc e => e -> FleetCtl -> W.Application
app env api req respond =
    case W.pathInfo req of
        ["terminal", ship] -> do
            ship <- readShip ship
            serveTerminal env api ship 0 req respond
        ["terminal", ship, session] -> do
            session :: Word <- evaluate $ read $ unpack session
            ship <- readShip ship
            serveTerminal env api ship session req respond
        ["status"] ->
            respond $ W.responseLBS H.status200 [] $ encode stubStatus
        _ ->
            respond $ W.responseLBS H.status404 [] "No implemented"
