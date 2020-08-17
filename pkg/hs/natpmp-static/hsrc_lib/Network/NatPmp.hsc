{-# LANGUAGE ForeignFunctionInterface, DeriveDataTypeable #-}

-- | This module is a thin wrapper above libnatpmp.h and getgateway.h.

module Network.NatPmp (Error(..),
                       NatPmpResponse(..),
                       ProtocolType(..),
                       NatPmpHandle,
                       Port,
                       LifetimeSeconds,
                       initNatPmp,
                       closeNatPmp,
                       getDefaultGateway,
                       getPublicAddress,
                       setPortMapping) where

#include <netinet/in.h>

#include <getgateway.h>
#include <natpmp.h>
#include <binding.h>

import Prelude
import Foreign
import Foreign.C
import Network.Socket

import Control.Monad.IO.Unlift (MonadIO(..))

-- Opaque type for the internals of nat pmp
data NatPmpStruct
type NatPmpHandle = Ptr NatPmpStruct

type Port = Word16
type LifetimeSeconds = Word32

-- The response type, in its internal form. This struct is a C tagged union
-- with additional data, but we need to read and write from its C form.
data NatPmpResponse
  = NatPmpResponsePublicAddress HostAddress
  | NatPmpResponseUdpPortMapping Port Port LifetimeSeconds
  | NatPmpResponseTcpPortMapping Port Port LifetimeSeconds
  deriving (Show)

instance Storable NatPmpResponse where
  sizeOf    _ = #{size natpmpresp_t}
  alignment _ = alignment (undefined :: CString)

  peek p = do
    t <- uintToEnum <$> (#{peek natpmpresp_t, type} p)
    case t of
      RTPublicAddress  ->
        NatPmpResponsePublicAddress <$>
          (#{peek natpmpresp_t, pnu.publicaddress.addr} p)
      RTUdpPortMapping ->
        NatPmpResponseUdpPortMapping
          <$> (#{peek natpmpresp_t, pnu.newportmapping.privateport} p)
          <*> (#{peek natpmpresp_t, pnu.newportmapping.mappedpublicport} p)
          <*> (#{peek natpmpresp_t, pnu.newportmapping.lifetime} p)
      RTTcpPortMapping ->
        NatPmpResponseTcpPortMapping
          <$> (#{peek natpmpresp_t, pnu.newportmapping.privateport} p)
          <*> (#{peek natpmpresp_t, pnu.newportmapping.mappedpublicport} p)
          <*> (#{peek natpmpresp_t, pnu.newportmapping.lifetime} p)

  poke _ _ = error "Responses are an output data structure; poke makes no sense"

type NatPmpResponseHandle = Ptr NatPmpResponse

foreign import ccall unsafe "getgateway.h getdefaultgateway" _get_default_gateway :: Ptr CUInt -> IO CInt

foreign import ccall unsafe "natpmp.h initnatpmp" _init_nat_pmp :: NatPmpHandle -> CInt -> CInt -> IO CInt
foreign import ccall unsafe "natpmp.h closenatpmp" _close_nat_pmp :: NatPmpHandle -> IO CInt
foreign import ccall unsafe "natpmp.h sendpublicaddressrequest" sendPublicAddressRequest :: NatPmpHandle -> IO CInt
foreign import ccall unsafe "natpmp.h sendnewportmappingrequest" sendNewPortMappingRequest :: NatPmpHandle -> CInt -> CUShort -> CUShort -> CUInt -> IO CInt

foreign import ccall unsafe "binding.h readNatResponseSynchronously" readNatResponseSynchronously :: NatPmpHandle -> NatPmpResponseHandle -> IO CInt

-- Give the type system some help
_peekCUInt :: Ptr CUInt -> IO CUInt
_peekCUInt = peek

uintToEnum :: Enum e => CUInt -> e
uintToEnum = toEnum . fromIntegral

intToEnum :: Enum e => CInt -> e
intToEnum = toEnum . fromIntegral


-- Fetches the default gateway as an ipv4 address
getDefaultGateway :: IO (Maybe HostAddress)
getDefaultGateway =
  alloca $ \(pReturnAddr :: Ptr CUInt) -> do
      _get_default_gateway pReturnAddr >>= \case
        0 -> (Just . fromIntegral) <$> _peekCUInt pReturnAddr
        _ -> pure Nothing


data RespType
  = RTPublicAddress
  | RTUdpPortMapping
  | RTTcpPortMapping
  deriving (Eq, Show)

instance Enum RespType where
  fromEnum RTPublicAddress = 0
  fromEnum RTUdpPortMapping = 1
  fromEnum RTTcpPortMapping = 2

  toEnum 0 = RTPublicAddress
  toEnum 1 = RTUdpPortMapping
  toEnum 2 = RTTcpPortMapping
  toEnum unmatched = error ("RespType.toEnum: Cannot match " ++ show unmatched)


data ProtocolType
  = PTUdp
  | PTTcp
  deriving (Eq, Show)

instance Enum ProtocolType where
  fromEnum PTUdp = 1
  fromEnum PTTcp = 2

  toEnum 1 = PTUdp
  toEnum 2 = PTTcp
  toEnum x = error ("ProtocolType.toEnum: Cannot match " ++ show x)


data Error
  = ErrInvalidArgs
  | ErrSocketError
  | ErrCannotGetGateway
  | ErrCloseErr
  | ErrRecvFrom
  | ErrNoPendingReq
  | ErrNoGatewaySupport
  | ErrConnectErr
  | ErrWrongPacketSource
  | ErrSendErr
  | ErrFcntlError
  | ErrGetTimeOfDayError
  --
  | ErrUnsuportedVersion
  | ErrUnsupportedOpcode
  --
  | ErrUndefinedError
  | ErrNotAuthorized
  | ErrNetworkFailure
  | ErrOutOfResources
  --
  | ErrTryAgain
  | ErrHaskellBindings
  deriving (Eq, Show)

instance Enum Error where
  fromEnum ErrInvalidArgs = -1
  fromEnum ErrSocketError = -2
  fromEnum ErrCannotGetGateway = -3
  fromEnum ErrCloseErr = -4
  fromEnum ErrRecvFrom = -5
  fromEnum ErrNoPendingReq = -6
  fromEnum ErrNoGatewaySupport = -7
  fromEnum ErrConnectErr = -8
  fromEnum ErrWrongPacketSource = -9
  fromEnum ErrSendErr = -10
  fromEnum ErrFcntlError = -11
  fromEnum ErrGetTimeOfDayError = -12
  --
  fromEnum ErrUnsuportedVersion = -14
  fromEnum ErrUnsupportedOpcode = -15
  --
  fromEnum ErrUndefinedError = -49
  fromEnum ErrNotAuthorized = -51
  fromEnum ErrNetworkFailure = -52
  fromEnum ErrOutOfResources = -53
  --
  fromEnum ErrTryAgain = -100
  fromEnum ErrHaskellBindings = -200

  toEnum (-1) = ErrInvalidArgs
  toEnum (-2) = ErrSocketError
  toEnum (-3) = ErrCannotGetGateway
  toEnum (-4) = ErrCloseErr
  toEnum (-5) = ErrRecvFrom
  toEnum (-6) = ErrNoPendingReq
  toEnum (-7) = ErrNoGatewaySupport
  toEnum (-8) = ErrConnectErr
  toEnum (-9) = ErrWrongPacketSource
  toEnum (-10) = ErrSendErr
  toEnum (-11) = ErrFcntlError
  toEnum (-12) = ErrGetTimeOfDayError
  --
  toEnum (-14) = ErrUnsuportedVersion
  toEnum (-15) = ErrUnsupportedOpcode
  --
  toEnum (-49) = ErrUndefinedError
  toEnum (-51) = ErrNotAuthorized
  toEnum (-52) = ErrNetworkFailure
  toEnum (-53) = ErrOutOfResources
  --
  toEnum (-100) = ErrTryAgain
  toEnum (-200) = ErrHaskellBindings
  toEnum unmatched = error ("Error.toEnum: Cannot match " ++ show unmatched)


initNatPmp :: MonadIO m => m (Either Error NatPmpHandle)
initNatPmp = liftIO do
  natpmp <- mallocBytes #{size natpmp_t}
  ret    <- _init_nat_pmp natpmp 0 0
  case ret of
    0 -> pure $ Right natpmp
    _ -> do
      free natpmp
      pure $ Left $ intToEnum ret


closeNatPmp :: MonadIO m => NatPmpHandle -> m (Either Error ())
closeNatPmp handle = liftIO do
  ret <- _close_nat_pmp handle
  free handle
  case ret of
    0 -> pure $ Right ()
    _ -> pure $ Left $ intToEnum ret


-- | Public interface for getting the public IPv4 address
getPublicAddress :: MonadIO m => NatPmpHandle -> m (Either Error HostAddress)
getPublicAddress natpmp = liftIO do
  sendRetcode <- sendPublicAddressRequest natpmp
  case sendRetcode of
    2 -> alloca $ \(pResponse :: NatPmpResponseHandle) -> do
      respRetcode <- readNatResponseSynchronously natpmp pResponse
      case respRetcode of
        0 -> peek pResponse >>= \case
          NatPmpResponsePublicAddress addr -> pure $ Right addr
          _ -> pure $ Left ErrHaskellBindings
        _ -> pure $ Left $ intToEnum respRetcode
    _ -> pure $ Left $ intToEnum sendRetcode

-- | Requests that the router maps the privatePort on our local computer in our
-- private network to publicPort on the public internet.
setPortMapping :: MonadIO m
               => NatPmpHandle
               -> ProtocolType
               -> Port
               -> Port
               -> LifetimeSeconds
               -> m (Either Error ())
setPortMapping natpmp protocol privatePort publicPort lifetime = liftIO do
  let protocolNum = fromEnum protocol
  sendResp <-
    sendNewPortMappingRequest natpmp
      (fromIntegral protocolNum) (CUShort privatePort) (CUShort publicPort)
      (CUInt lifetime)

  case sendResp of
    12 -> alloca $ \(pResponse :: NatPmpResponseHandle) -> do
      respRetcode <- readNatResponseSynchronously natpmp pResponse
      case respRetcode of
        0 -> peek pResponse >>= \case
          NatPmpResponseUdpPortMapping _ _ _ -> pure $ Right ()
          NatPmpResponseTcpPortMapping _ _ _ -> pure $ Right ()
          _ -> pure $ Left ErrHaskellBindings
        _ -> pure $ Left $ intToEnum respRetcode
    x -> pure $ Left $ intToEnum x
