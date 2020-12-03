{-|
    Use websockets to pass nouns between a client and server.
-}

module Urbit.Vere.NounServ
    ( Conn(..)
    , Server(..)
    , Client(..)
    , wsServer
    , wsClient
    , testIt
    , wsServApp
    , mkConn
    , wsConn
    ) where

import Urbit.Prelude

import qualified Network.Wai.Handler.Warp as W
import qualified Network.WebSockets       as WS

--------------------------------------------------------------------------------

data Conn i o = Conn
    { cRecv :: STM (Maybe i)
    , cSend :: o -> STM ()
    }

mkConn :: TBMChan i -> TBMChan o -> Conn i o
mkConn inp out = Conn (readTBMChan inp) (writeTBMChan out)

--------------------------------------------------------------------------------

data Client i o = Client
    { cConn  :: Conn i o
    , cAsync :: Async ()
    }

data Server i o a = Server
    { sAccept :: STM (Maybe (Conn i o))
    , sAsync  :: Async ()
    , sData   :: a
    }

--------------------------------------------------------------------------------

withRIOThread :: RIO e a -> RIO e (Async a)
withRIOThread act = do
    env <- ask
    io $ async $ runRIO env $ act

wsConn :: (FromNoun i, ToNoun o, Show i, Show o, HasLogFunc e)
       => Utf8Builder
       -> TBMChan i -> TBMChan o
       -> WS.Connection
       -> RIO e ()
wsConn pre inp out wsc = do
    logDebug (pre <> "(wcConn) Connected!")

    writer <- withRIOThread $ forever $ do
        logDebug (pre <> "(wsConn) Waiting for data.")
        byt <- io $ toStrict <$> WS.receiveData wsc
        logDebug (pre <> "Got data")
        dat <- cueBSExn byt >>= fromNounExn
        logDebug (pre <> "(wsConn) Decoded data, writing to chan")
        atomically $ writeTBMChan inp dat

    reader <- withRIOThread $ forever $ do
        logDebug (pre <> "Waiting for data from chan")
        atomically (readTBMChan out) >>= \case
            Nothing  -> do
                logDebug (pre <> "(wsConn) Connection closed")
                error "dead-conn"
            Just msg -> do
                logDebug (pre <> "(wsConn) Got message! " <> displayShow msg)
                io $ WS.sendBinaryData wsc $ fromStrict $ jamBS $ toNoun msg

    let cleanup = do
            atomically (closeTBMChan inp >> closeTBMChan out)
            cancel writer
            cancel reader

    flip finally cleanup $ do
         res <- atomically (waitCatchSTM writer <|> waitCatchSTM reader)
         logInfo $ displayShow (res :: Either SomeException ())


--------------------------------------------------------------------------------

wsClient :: forall i o e. (ToNoun o, FromNoun i, Show o, Show i, HasLogFunc e)
         => Text -> W.Port -> RIO e (Client i o)
wsClient pax por = do
    env <- ask
    inp <- io $ newTBMChanIO 5
    out <- io $ newTBMChanIO 5
    con <- pure (mkConn inp out)

    logInfo "NOUNSERV (wsClie) Trying to connect"

    tid <- io $ async
              $ WS.runClient "127.0.0.1" por (unpack pax)
              $ \con -> WS.withPingThread con 15 (pure ()) $
                            runRIO env (wsConn "NOUNSERV (wsClie) " inp out con)

    pure $ Client con tid

--------------------------------------------------------------------------------

wsServApp :: (HasLogFunc e, ToNoun o, FromNoun i, Show i, Show o)
          => (Conn i o -> STM ())
          -> WS.PendingConnection
          -> RIO e ()
wsServApp cb pen = do
    logInfo "NOUNSERV (wsServer) Got connection!"
    wsc <- io $ WS.acceptRequest pen
    inp <- io $ newTBMChanIO 5
    out <- io $ newTBMChanIO 5
    atomically $ cb (mkConn inp out)
    wsConn "NOUNSERV (wsServ) " inp out wsc

wsServer :: forall i o e. (ToNoun o, FromNoun i, Show i, Show o, HasLogFunc e)
         => RIO e (Server i o W.Port)
wsServer = do
    con <- io $ newTBMChanIO 5

    tid <- async $ do
        env <- ask
        logInfo "NOUNSERV (wsServer) Starting server"
        io $ WS.runServer "127.0.0.1" 9999
           $ runRIO env . wsServApp (writeTBMChan con)
        logInfo "NOUNSERV (wsServer) Server died"
        atomically $ closeTBMChan con

    pure $ Server (readTBMChan con) tid 9999


-- Hacky Integration Test ------------------------------------------------------

fromJust :: MonadIO m => Text -> Maybe a -> m a
fromJust err Nothing  = error (unpack err)
fromJust _   (Just x) = pure x

type Example = Maybe (Word, (), Word)

example :: Example
example = Just (99, (), 44)

testIt :: HasLogFunc e => RIO e ()
testIt = do
    logDebug "(testIt) Starting Server"
    Server{..} <- wsServer @Example @Example
    logDebug "(testIt) Connecting"
    Client{..} <- wsClient @Example @Example "/" sData

    logDebug "(testIt) Accepting connection"
    sConn <- fromJust "accept" =<< atomically sAccept

    let
        clientSend = do
            logDebug "(testIt) Sending from client"
            atomically (cSend cConn example)
            logDebug "(testIt) Waiting for response"
            res <- atomically (cRecv sConn)
            print ("clientSend", res, example)
            unless (res == Just example) $ do
                error "Bad data"
            logDebug "(testIt) Success"

        serverSend = do
            logDebug "(testIt) Sending from server"
            atomically (cSend sConn example)
            logDebug "(testIt) Waiting for response"
            res <- atomically (cRecv cConn)
            print ("serverSend", res, example)
            unless (res == Just example) $ do
                error "Bad data"
            logDebug "(testIt) Success"

    clientSend
    clientSend
    clientSend
    serverSend
    serverSend

    cancel sAsync
    cancel cAsync
