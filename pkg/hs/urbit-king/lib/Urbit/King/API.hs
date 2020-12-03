{-|
    TODO This has a bunch of stub logic that was intended for an
         architecture with a single Urbit daemon running multiple
         ships. Do it or strip it out.
-}

module Urbit.King.API
  ( King(..)
  , TermConn
  , TermConnAPI
  , kingAPI
  , readPortsFile
  )
where

import RIO.Directory
import Urbit.Prelude

import Network.Socket (Socket)
import Prelude        (read)
import Urbit.King.App (HasPierPath(..))

import qualified Network.HTTP.Types             as H
import qualified Network.Wai                    as W
import qualified Network.Wai.Handler.Warp       as W
import qualified Network.Wai.Handler.WebSockets as WS
import qualified Network.WebSockets             as WS
import qualified Urbit.Vere.NounServ            as NounServ
import qualified Urbit.Vere.Term.API            as Term


-- Types -----------------------------------------------------------------------

type TermConn = NounServ.Conn Term.ClientTake [Term.Ev]

type TermConnAPI = TVar (Maybe (TermConn -> STM ()))

{-|
    Daemon state.
-}
data King = King
    { kServer   :: Async ()
    , kTermConn :: TermConnAPI
    }


--------------------------------------------------------------------------------

{-|
    Get the filepath of the urbit config directory and the ports file.
-}
portsFilePath :: HasPierPath e => RIO e (FilePath, FilePath)
portsFilePath = do
    dir <- view pierPathL
    fil <- pure (dir </> ".king.ports")
    pure (dir, fil)

{-|
    Write the ports file.
-}
portsFile :: HasPierPath e => Word -> RAcquire e (FilePath, FilePath)
portsFile por =
    mkRAcquire mkFile (removeFile . snd)
  where
    mkFile = do
        (dir, fil) <- portsFilePath
        createDirectoryIfMissing True dir
        writeFile fil (encodeUtf8 $ tshow por)
        pure (dir, fil)

{-|
    Get the HTTP port for the running Urbit daemon.
-}
readPortsFile :: HasPierPath e => RIO e (Maybe Word)
readPortsFile = do
    (_, fil) <- portsFilePath
    bs <- readFile fil
    evaluate (readMay $ unpack $ decodeUtf8 bs)

kingServer :: HasLogFunc e => (Int, Socket) -> RAcquire e King
kingServer is =
    mkRAcquire (startKing is) (cancel . kServer)
  where
    startKing :: HasLogFunc e => (Int, Socket) -> RIO e King
    startKing (port, sock) = do
        api <- newTVarIO Nothing
        let opts = W.defaultSettings & W.setPort port
        env <- ask
        tid <- async $ io $ W.runSettingsSocket opts sock $ app env api
        pure (King tid api)

{-|
    Start the HTTP server and write to the ports file.
-}
kingAPI :: (HasPierPath e, HasLogFunc e)
        => RAcquire e King
kingAPI = do
    (port, sock) <- io $ W.openFreePort
    (dir, fil)   <- portsFile (fromIntegral port)
    -- lockFile dir
    kingServer (port, sock)

serveTerminal :: HasLogFunc e => e -> TermConnAPI -> Word -> W.Application
serveTerminal env api word =
    WS.websocketsOr WS.defaultConnectionOptions wsApp fallback
  where
    fallback req respond =
        respond $ W.responseLBS H.status500 []
                $ "This endpoint uses websockets"

    wsApp pen =
        atomically (readTVar api) >>= \case
            Nothing -> WS.rejectRequest pen "Ship not running"
            Just sp -> do
                wsc <- io $ WS.acceptRequest pen
                inp <- io $ newTBMChanIO 5
                out <- io $ newTBMChanIO 5
                atomically $ sp $ NounServ.mkConn inp out
                let doit = runRIO env
                         $ NounServ.wsConn "NOUNSERV (wsServ) " inp out wsc

                -- If `wai` kills this thread for any reason, the TBMChans
                -- need to be closed. If they are not closed, the
                -- terminal will not know that they disconnected.
                finally doit $ atomically $ do
                    closeTBMChan inp
                    closeTBMChan out

data BadShip = BadShip Text
  deriving (Show, Exception)

app :: HasLogFunc e => e -> TermConnAPI -> W.Application
app env api req respond =
    case W.pathInfo req of
        ["terminal", session] -> do
            session :: Word <- evaluate $ read $ unpack session
            serveTerminal env api session req respond
        ["status"] ->
            respond $ W.responseLBS H.status200 [] "{}"
        _ ->
            respond $ W.responseLBS H.status404 [] "No implemented"
