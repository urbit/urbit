{-|
  Eyre: Http Server Driver
-}

module Urbit.Vere.Eyre.Multi
  ( WhichServer(..)
  , MultiEyreConf(..)
  , OnMultiReq
  , OnMultiKil
  , MultiEyreApi(..)
  , joinMultiEyre
  , leaveMultiEyre
  , multiEyre
  )
where

import Urbit.Prelude hiding (Builder)

import Urbit.Arvo           hiding (ServerId, reqUrl)
import Urbit.Vere.Eyre.Serv
import Urbit.Vere.Eyre.Wai

import Network.TLS                 (Credential)
import Urbit.Vere.Eyre.KingSubsite (KingSubsite, fourOhFourSubsite)


-- Types -----------------------------------------------------------------------

data WhichServer = Secure | Insecure | Loopback
  deriving (Eq)

data MultiEyreConf = MultiEyreConf
  { mecHttpsPort :: Maybe Port
  , mecHttpPort :: Maybe Port
  , mecLocalhostOnly :: Bool
  }
 deriving (Show)

type OnMultiReq = WhichServer -> Ship -> Word64 -> ReqInfo -> STM ()

type OnMultiKil = Ship -> Word64 -> STM ()

data MultiEyreApi = MultiEyreApi
  { meaConf :: MultiEyreConf
  , meaLive :: TVar LiveReqs
  , meaPlan :: TVar (Map Ship OnMultiReq)
  , meaCanc :: TVar (Map Ship OnMultiKil)
  , meaTlsC :: TVar (Map Ship (TlsConfig, Credential))
  , meaSite :: TVar (Map Ship KingSubsite)
  , meaKill :: IO ()
  }


-- Multi-Tenet HTTP ------------------------------------------------------------

joinMultiEyre
  :: MultiEyreApi
  -> Ship
  -> Maybe (TlsConfig, Credential)
  -> OnMultiReq
  -> OnMultiKil
  -> KingSubsite
  -> STM ()
joinMultiEyre api who mTls onReq onKil sub = do
  modifyTVar' (meaPlan api) (insertMap who onReq)
  modifyTVar' (meaCanc api) (insertMap who onKil)
  for_ mTls $ \creds -> do
    modifyTVar' (meaTlsC api) (insertMap who creds)
  modifyTVar' (meaSite api) (insertMap who sub)

leaveMultiEyre :: MultiEyreApi -> Ship -> STM ()
leaveMultiEyre MultiEyreApi {..} who = do
  modifyTVar' meaCanc (deleteMap who)
  modifyTVar' meaPlan (deleteMap who)
  modifyTVar' meaTlsC (deleteMap who)
  modifyTVar' meaSite (deleteMap who)

multiEyre :: HasLogFunc e => IO () -> MultiEyreConf -> RIO e MultiEyreApi
multiEyre onFatal conf@MultiEyreConf {..} = do
  logInfo (displayShow ("EYRE", "MULTI", conf))

  vLive <- io emptyLiveReqs >>= newTVarIO
  vPlan <- newTVarIO mempty
  vCanc <- newTVarIO (mempty :: Map Ship (Ship -> Word64 -> STM ()))
  vTlsC <- newTVarIO mempty
  vSite <- newTVarIO mempty

  let site :: Ship -> STM KingSubsite
      site who = do
        sites <- readTVar vSite
        pure $ maybe (fourOhFourSubsite who) id $ lookup who sites

  let host = if mecLocalhostOnly then SHLocalhost else SHAnyHostOk

  let onReq :: WhichServer -> Ship -> Word64 -> ReqInfo -> STM ()
      onReq which who reqId reqInfo = do
        plan <- readTVar vPlan
        lookup who plan & \case
          Nothing -> pure ()
          Just cb -> cb which who reqId reqInfo

  let onKil :: Ship -> Word64 -> STM ()
      onKil who reqId = do
        canc <- readTVar vCanc
        lookup who canc & \case
          Nothing -> pure ()
          Just cb -> cb who reqId

  mIns <- for mecHttpPort $ \por -> do
    logInfo (displayShow ("EYRE", "MULTI", "HTTP", por))
    serv vLive onFatal $ ServConf
      { scHost = host
      , scPort = SPChoices $ singleton $ fromIntegral por
      , scRedi = Nothing -- TODO
      , scFake = False
      , scType = STMultiHttp site $ ReqApi
          { rcReq = onReq Insecure
          , rcKil = onKil
          }
      }

  mSec <- for mecHttpsPort $ \por -> do
    logInfo (displayShow ("EYRE", "MULTI", "HTTPS", por))
    serv vLive onFatal $ ServConf
      { scHost = host
      , scPort = SPChoices $ singleton $ fromIntegral por
      , scRedi = Nothing
      , scFake = False
      , scType = STMultiHttps (MTC vTlsC) site $ ReqApi
          { rcReq = onReq Secure
          , rcKil = onKil
          }
      }

  pure $ MultiEyreApi
    { meaLive = vLive
    , meaPlan = vPlan
    , meaCanc = vCanc
    , meaTlsC = vTlsC
    , meaSite = vSite
    , meaConf = conf
    , meaKill = traverse_ saKil (toList mIns <> toList mSec)
    }
