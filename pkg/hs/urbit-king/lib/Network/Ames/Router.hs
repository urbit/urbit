module Network.Ames.Router (buildRouter) where

import Urbit.Prelude

import Network.Ames.Types

import qualified Data.Map as M

-- Runtime details of the router.
data RouterImpl = RouterImpl
  { riShips :: TVar (Map ShipLife Writer)
  , riKeys  :: TVar (Map ShipLife ByteString)
  }

-- Writer interface implementation --------------------------------------------

writerSend :: RouterImpl -> WriterSendMsg
writerSend impl src dst@(MsgDest shipLife) msg = do
  -- First, check to see if the destination is another Writer on this same
  -- Router. This happens in the multi-tennat case and means we don't need to
  -- perform any encryption, or deal with any of the main
  ships <- atomically $ readTVar (riShips impl)
  case M.lookup shipLife ships of
    Just writer -> (wRecvMsg writer) src dst msg >>= newTMVarIO
    Nothing -> error "Need to handle communication over transports"


writerJoinRouter :: RouterImpl -> WriterJoinRouter
writerJoinRouter impl writer ship = do
  let key = (wPrivateKey writer) ship

  atomically $ do
    modifyTVar (riShips impl) (M.insert ship writer)
    modifyTVar (riKeys impl) (M.insert ship key)

  -- Now that the Router has all the information about this writer, give the
  -- writer a chance to give commands.
  (wRestart writer)


writerLeaveRouter :: RouterImpl -> WriterLeaveRouter
writerLeaveRouter impl ship = do
  atomically $ modifyTVar (riShips impl) (M.delete ship)





-- Start a router, passing the two api handles to add transports and writers to
-- the router.
buildRouter :: RAcquire e (AmesTransportRouterApi, AmesRouterWriterApi)
buildRouter =
  do
    impl <- mkRAcquire buildImpl destroyImpl
    pure (transportApi, routerApi impl)
  where
    buildImpl = do
      ships <- newTVarIO mempty
      keys  <- newTVarIO mempty
      pure $ RouterImpl ships keys

    destroyImpl impl = pure ()

    transportApi = AmesTransportRouterApi

    routerApi impl = AmesRouterWriterApi
      { arwaSend = (writerSend impl)
      , arwaJoinRouter = (writerJoinRouter impl)
      , arwaLeaveRouter = (writerLeaveRouter impl)
      }

{-

buildEverything env ship = do
  (transportApi, writerApi) <- buildRouter

  -- transport data structures are going to start up their subsystems behind
  -- the scenes. () is returned since 
  httpTransport transportApi
  -- torTransport transportApi

  -- At this point, we have a real router. The next thing is to connect some
  -- Writers to it. In the context of an instance of king running one or more
  -- Urbit ships using nockWriter, nockWriter returns an effect handling
  -- function like all the other IO drivers.

  -- The NockWriter communicates 
  theEffectFunctionForAShip <- nockWriter env ship writerApi

-}

