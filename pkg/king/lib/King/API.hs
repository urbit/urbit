module King.API (kingAPI, readPortsFile) where

import UrbitPrelude
import Data.Aeson
import RIO.Directory

import King.App       (HasConfigDir(..))
import Network.Socket (Socket)
import Prelude        (read)
import Vere.LockFile  (lockFile)

import qualified Network.HTTP.Types             as H
import qualified Network.Wai                    as W
import qualified Network.Wai.Handler.Warp       as W
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets             as WS
import qualified Urbit.Ob                       as Ob


-- Types -----------------------------------------------------------------------

{-
    Daemon state.
-}
data King = King
    { kServer :: Async ()
    }

data ShipStatus = Halted | Booting | Booted | Running | LandscapeUp
  deriving (Generic, ToJSON, FromJSON)

data KingStatus = Starting | Started
  deriving (Generic, ToJSON, FromJSON)

data StatusResp = StatusResp
    { king  :: KingStatus
    , ships :: Map Text ShipStatus
    }
  deriving (Generic, ToJSON, FromJSON)


--------------------------------------------------------------------------------

{-
    Get the filepath of the urbit config directory and the ports file.
-}
portsFilePath :: HasConfigDir e => RIO e (FilePath, FilePath)
portsFilePath = do
    dir <- view configDirL
    fil <- pure (dir </> ".king.ports")
    pure (dir, fil)

{-
    Write the ports file.
-}
portsFile :: HasConfigDir e => Word -> RAcquire e (FilePath, FilePath)
portsFile por =
    mkRAcquire mkFile (removeFile . snd)
  where
    mkFile = do
        (dir, fil) <- portsFilePath
        createDirectoryIfMissing True dir
        writeFile fil (encodeUtf8 $ tshow por)
        pure (dir, fil)

{-
    Get the HTTP port for the running Urbit daemon.
-}
readPortsFile :: HasConfigDir e => RIO e (Maybe Word)
readPortsFile = do
    (_, fil) <- portsFilePath
    bs <- readFile fil
    evaluate (read $ unpack $ decodeUtf8 bs)

kingServer :: HasLogFunc e => (Int, Socket) -> RAcquire e King
kingServer is =
    mkRAcquire (startKing is) (cancel . kServer)
  where
    startKing :: HasLogFunc e => (Int, Socket) -> RIO e King
    startKing (port, sock) = do
        let opts = W.defaultSettings & W.setPort port
        tid <- async $ io $ W.runSettingsSocket opts sock $ app
        pure (King tid)

{-
    Start the HTTP server and write to the ports file.
-}
kingAPI :: (HasConfigDir e, HasLogFunc e)
        => RAcquire e King
kingAPI = do
    (port, sock) <- io $ W.openFreePort
    (dir, fil)   <- portsFile (fromIntegral port)
    lockFile dir
    kingServer (port, sock)

stubStatus :: StatusResp
stubStatus = StatusResp Started $ mapFromList [("zod", Running)]

serveTerminal :: Ship -> Word -> W.Application
serveTerminal ship word =
    WS.websocketsOr WS.defaultConnectionOptions placeholderWSApp fallback
  where
    fallback req respond =
        respond $ W.responseLBS H.status500 []
                $ "This endpoint uses websockets"

placeholderWSApp :: WS.ServerApp
placeholderWSApp _ = pure ()

data BadShip = BadShip Text
  deriving (Show, Exception)

readShip :: Text -> IO Ship
readShip t = Ob.parsePatp t & \case
     Left err -> throwIO (BadShip t)
     Right pp -> pure $ Ship $ fromIntegral $ Ob.fromPatp pp

app :: W.Application
app req respond =
    case W.pathInfo req of
        ["terminal", ship, session] -> do
            session :: Word <- evaluate $ read $ unpack session
            ship <- readShip ship
            serveTerminal ship session req respond
        ["status"] ->
            respond $ W.responseLBS H.status200 [] $ encode stubStatus
        _ ->
            respond $ W.responseLBS H.status404 [] "No implemented"
