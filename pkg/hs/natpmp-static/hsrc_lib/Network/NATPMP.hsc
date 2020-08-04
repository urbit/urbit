{-# LANGUAGE ForeignFunctionInterface, DeriveDataTypeable #-}

-- | This module is a thin wrapper above libnatpmp.h and getgateway.h.

module Network.NATPMP (Error(..),
                        NatPmpResponse(..),
                        ProtocolType(..),
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


-- Opaque type for the internals of nat pmp
data NatPmpStruct
type NatPmpHandle = Ptr NatPmpStruct

-- The response type, in its internal form. This struct is a C tagged union
-- with additional data, but we need to read and write from its C form.
--
-- TODO: What's easier? Exposing the internal C sum type here using Storable,
-- or manual translation?
data NatPmpResponse
  = NatPmpResponsePublicAddress HostAddress
  | NatPmpResponseUDPPortMapping Word16 Word16 Word32
  | NatPmpResponseTCPPortMapping Word16 Word16 Word32
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
      RTUDPPortMapping ->
        NatPmpResponseUDPPortMapping
          <$> (#{peek natpmpresp_t, pnu.newportmapping.privateport} p)
          <*> (#{peek natpmpresp_t, pnu.newportmapping.mappedpublicport} p)
          <*> (#{peek natpmpresp_t, pnu.newportmapping.lifetime} p)
      RTTCPPortMapping ->
        NatPmpResponseTCPPortMapping
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

--foreign import ccall unsafe "binding.h reatNatResponseSynchronously" 

-- Give the type system some help
_peekCUInt :: Ptr CUInt -> IO CUInt
_peekCUInt = peek

uintToEnum :: Enum e => CUInt -> e
uintToEnum = toEnum . fromIntegral

intToEnum :: Enum e => CInt -> e
intToEnum = toEnum . fromIntegral

-- intFromEnum :: Enum e => e -> CInt
-- intFromEnum = fromIntegral . fromEnum


-- Fetches the default gateway as an ipv4 address
getDefaultGateway :: IO (Maybe HostAddress)
getDefaultGateway =
  alloca $ \(pReturnAddr :: Ptr CUInt) -> do
      ret <- _get_default_gateway pReturnAddr
      case ret of
        0 -> (Just . fromIntegral) <$> _peekCUInt pReturnAddr
        _ -> pure Nothing

-- TODO: Unsure about how to actually bind this library together. So RespType
-- is an enum which is just the integer in a low level 

data RespType
  = RTPublicAddress
  | RTUDPPortMapping
  | RTTCPPortMapping
  deriving (Eq, Show)

instance Enum RespType where
  fromEnum RTPublicAddress = 0
  fromEnum RTUDPPortMapping = 1
  fromEnum RTTCPPortMapping = 2

  toEnum 0 = RTPublicAddress
  toEnum 1 = RTUDPPortMapping
  toEnum 2 = RTTCPPortMapping
  toEnum unmatched = error ("RespType.toEnum: Cannot match " ++ show unmatched)

data ProtocolType
  = PTUDP
  | PTTCP
  deriving (Eq, Show)

instance Enum ProtocolType where
  fromEnum PTUDP = 1
  fromEnum PTTCP = 2

  toEnum 1 = PTUDP
  toEnum 2 = PTTCP
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
  | ErrHaskellBindingsErr
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
  fromEnum ErrHaskellBindingsErr = -200

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
  toEnum (-200) = ErrHaskellBindingsErr
  toEnum unmatched = error ("Error.toEnum: Cannot match " ++ show unmatched)


initNatPmp :: IO (Either Error NatPmpHandle)
initNatPmp = do
  natpmp  <- mallocBytes #{size natpmp_t}
  ret <- _init_nat_pmp natpmp 0 0
  case ret of
    0 -> pure $ Right natpmp
    _ -> do
      free natpmp
      pure $ Left $ intToEnum ret

closeNatPmp :: NatPmpHandle -> IO (Either Error ())
closeNatPmp handle = do
  ret <- _close_nat_pmp handle
  free handle
  case ret of
    0 -> pure $ Right ()
    _ -> pure $ Left $ intToEnum ret


-- Public interface for getting the public IPv4 address
getPublicAddress :: NatPmpHandle -> IO (Either Error HostAddress)
getPublicAddress natpmp = do
  sendRetcode <- sendPublicAddressRequest natpmp
  case sendRetcode of
    2 -> alloca $ \(pResponse :: NatPmpResponseHandle) -> do
      respRetcode <- readNatResponseSynchronously natpmp pResponse
      case respRetcode of
        0 -> peek pResponse >>= \case
          NatPmpResponsePublicAddress addr -> pure $ Right addr
          _ -> pure $ Left ErrHaskellBindingsErr
        _ -> pure $ Left $ intToEnum respRetcode
    _ -> pure $ Left $ intToEnum sendRetcode


setPortMapping :: NatPmpHandle -> ProtocolType -> Word16 -> Word16 -> Word32
               -> IO (Either Error ())
setPortMapping natpmp protocol privatePort publicPort lifetime = do
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
          NatPmpResponseUDPPortMapping _ _ _ -> pure $ Right ()
          NatPmpResponseTCPPortMapping _ _ _ -> pure $ Right ()
          _ -> pure $ Left ErrHaskellBindingsErr
        _ -> pure $ Left $ intToEnum respRetcode
    x -> pure $ Left $ intToEnum x
