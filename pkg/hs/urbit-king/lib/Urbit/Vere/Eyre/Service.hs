{-|
  Eyre: Http Server Driver
-}

module Urbit.Vere.Eyre.Service
  ( restartService
  , stopService
  )
where

import Urbit.Prelude


-- Generic Service Stop/Restart -- Using an MVar for Atomicity -----------------

{-|
    Restart a running service.

    This can probably be made simpler, but it

    - Sets the MVar to Nothing if there was an exception while starting
      or stopping the service.

    - Keeps the MVar lock until the restart process finishes.
-}
restartService
  :: forall e s
   . HasLogFunc e
  => MVar (Maybe s)
  -> RIO e s
  -> (s -> RIO e ())
  -> RIO e (Either SomeException s)
restartService vServ sstart kkill = do
  logInfo "restartService"
  modifyMVar vServ $ \case
    Nothing -> doStart
    Just sv -> doRestart sv
 where
  doRestart :: s -> RIO e (Maybe s, Either SomeException s)
  doRestart serv = do
    logInfo "doStart"
    try (kkill serv) >>= \case
      Left  exn -> pure (Nothing, Left exn)
      Right ()  -> doStart

  doStart :: RIO e (Maybe s, Either SomeException s)
  doStart = do
    logInfo "doStart"
    try sstart <&> \case
      Right s   -> (Just s, Right s)
      Left  exn -> (Nothing, Left exn)

{-|
  Stop a running service. Do nothing if it's already stopped.
-}
stopService
  :: HasLogFunc e
  => MVar (Maybe s)
  -> (s -> RIO e ())
  -> RIO e (Either SomeException ())
stopService vServ kkill = do
  logInfo "stopService"
  modifyMVar vServ $ \case
    Nothing -> pure (Nothing, Right ())
    Just sv -> do
      res <- try (kkill sv)
      pure (Nothing, res)
