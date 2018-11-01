module UV where

import Data.Tuple.Nested
import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (ExceptT(..), withExceptT)
import Control.Monad.Trans.Class (lift)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust)
import Data.Symbol (SProxy(..))
import Data.Variant (Variant)
import Data.Variant as V
import Effect (Effect)
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)

foreign import data Buffer :: Type

type Error =
  Int

type Handler es a =
  ExceptT (Variant es) Effect a

errCode
  :: Variant
      ( run :: Error
      , tcpNew :: Error
      , tcpBind :: Error
      , tcpConnect :: Error
      , udpNew :: Error
      , udpBind :: Error
      , udpRecvStart :: Error
      , udpSetBroadcast :: Error
      , udpSend :: Error
      , listen :: Error
      , readStart :: Error
      )
  -> Error
errCode e =
  V.unvariant e # \(V.Unvariant k) ->
    k \_ v ->
      unsafeCoerce v

foreign import strerror :: Error -> String

foreign import data RunMode :: Type
foreign import _RunDefault :: RunMode
foreign import _RunOnce :: RunMode
foreign import _RunNoWait :: RunMode

foreign import data Loop :: Type
foreign import defaultLoop :: Effect Loop
foreign import newLoop :: Effect Loop
foreign import data LoopOption :: Type

_run :: SProxy "run"
_run = SProxy

run
  :: ∀ es
   . Loop
  -> RunMode
  -> Handler (run :: Error | es) Unit
run loop mode =
  withExceptT (V.inj _run) $
    ExceptT $
      runImpl Left Right loop mode

foreign import runImpl
  :: (∀ a b. a -> Either a b)
  -> (∀ a b. b -> Either a b)
  -> Loop
  -> RunMode
  -> Effect (Either Error Unit)

--------------------------------------------------------------------------------
-- Streams (TODO)
--------------------------------------------------------------------------------

foreign import data StreamHandle :: Type

class IsStreamHandle h where
  toStreamHandle :: h -> StreamHandle

instance isStreamHandleTcpHandle :: IsStreamHandle TcpHandle where
  toStreamHandle = unsafeCoerce

newtype Backlog =
  Backlog
    Int

_listen :: SProxy "listen"
_listen = SProxy

listen
  :: ∀ h es
   . IsStreamHandle h
  => Backlog
  -> (Either Error h -> Effect Unit)
  -> h
  -> Handler (listen :: Error | es) Unit
listen backlog cb h =
  withExceptT (V.inj _listen) $
    ExceptT $ listenImpl backlog cb $ toStreamHandle h

foreign import listenImpl
  :: ∀ h
   . Backlog
  -> (Either Error h -> Effect Unit)
  -> StreamHandle
  -> Effect (Either Error Unit)

_readStart :: SProxy "readStart"
_readStart = SProxy

readStart
  :: ∀ h es
   . IsStreamHandle h
  => (Unit -> Effect Unit)
  -> h
  -> Handler (readStart :: Error | es) Unit
readStart cb h =
  withExceptT (V.inj _readStart) $
    ExceptT $ readStartImpl cb $ toStreamHandle h

foreign import readStartImpl
  :: ∀ es
   . (Unit -> Effect Unit)
  -> StreamHandle
  -> Effect (Either Error Unit)

--------------------------------------------------------------------------------
-- Buffers
--------------------------------------------------------------------------------

foreign import bufferFromString :: String -> Effect Buffer
foreign import bufferToString :: Buffer -> Effect String

--------------------------------------------------------------------------------
-- Networking
--------------------------------------------------------------------------------

foreign import data SockAddrIn :: Type
foreign import data SockAddr :: Type

class IsSockAddr a where
  toSockAddr :: a -> SockAddr

instance isSockAddrSockAddrIn :: IsSockAddr SockAddrIn where
  toSockAddr = unsafeCoerce

type Ip = String
type Port = Int

foreign import ip4Addr :: Ip -> Port -> SockAddrIn

--------------------------------------------------------------------------------
-- Networking: TCP
--------------------------------------------------------------------------------

foreign import data TcpHandle :: Type
foreign import data TcpFlag :: Type

foreign import _TcpIpv6only :: TcpFlag

_tcpNew :: SProxy "tcpNew"
_tcpNew = SProxy

tcpNew
  :: ∀ es
   . Loop
  -> Handler (tcpNew :: Error | es) TcpHandle
tcpNew loop =
  withExceptT (V.inj _tcpNew) $
    ExceptT $
      tcpNewImpl Left Right Nothing Just loop

foreign import tcpNewImpl
  :: (∀ a b. a -> Either a b)
  -> (∀ a b. b -> Either a b)
  -> (∀ a. Maybe a)
  -> (∀ a. a -> Maybe a)
  -> Loop
  -> Effect (Either Error TcpHandle)

_tcpBind :: SProxy "tcpBind"
_tcpBind = SProxy

tcpBind
  :: ∀ es
   . SockAddrIn
  -> Array TcpFlag
  -> TcpHandle
  -> Handler (tcpBind :: Error | es) Unit
tcpBind addr flags handle =
  withExceptT (V.inj _tcpBind) $
    ExceptT $
      tcpBindImpl addr flags handle

foreign import tcpBindImpl
  :: SockAddrIn
  -> Array TcpFlag
  -> TcpHandle
  -> Effect (Either Error Unit)

_tcpConnect :: SProxy "tcpConnect"
_tcpConnect = SProxy

tcpConnect
  :: ∀ es sockAddr
   . IsSockAddr sockAddr
  => sockAddr
  -> (Either Error Unit -> Effect Unit)
  -> TcpHandle
  -> Handler (tcpConnect :: Error | es) Unit
tcpConnect addr cb handle =
  withExceptT (V.inj _tcpConnect) $
    ExceptT $
      tcpConnectImpl (toSockAddr addr) cb handle

foreign import tcpConnectImpl
  :: SockAddr
  -> (Either Error Unit -> Effect Unit)
  -> TcpHandle
  -> Effect (Either Error Unit)

--------------------------------------------------------------------------------
-- Networking: UDP
--------------------------------------------------------------------------------

foreign import data UdpHandle :: Type
foreign import data UdpFlag :: Type

foreign import _UdpIpv6only :: UdpFlag
foreign import _UdpPartial :: UdpFlag
foreign import _UdpReuseAddr :: UdpFlag

_udpNew :: SProxy "udpNew"
_udpNew = SProxy

udpNew
  :: ∀ es
   . Loop
  -> Handler (udpNew :: Error | es) UdpHandle
udpNew loop =
  withExceptT (V.inj _udpNew) $
    ExceptT $
      udpNewImpl Left Right Nothing Just loop

foreign import udpNewImpl
  :: (∀ a b. a -> Either a b)
  -> (∀ a b. b -> Either a b)
  -> (∀ a. Maybe a)
  -> (∀ a. a -> Maybe a)
  -> Loop
  -> Effect (Either Error UdpHandle)

_udpBind :: SProxy "udpBind"
_udpBind = SProxy

udpBind
  :: ∀ es
   . SockAddrIn
  -> Array UdpFlag
  -> UdpHandle
  -> Handler (udpBind :: Error | es) Unit
udpBind addr flags handle =
  withExceptT (V.inj _udpBind) $
    ExceptT $
      udpBindImpl addr flags handle

foreign import udpBindImpl
  :: SockAddrIn
  -> Array UdpFlag
  -> UdpHandle
  -> Effect (Either Error Unit)

_udpRecvStart :: SProxy "udpRecvStart"
_udpRecvStart = SProxy

udpRecvStart
  :: ∀ es
   . (Maybe Buffer -> Effect Unit)
  -> UdpHandle
  -> Handler (udpRecvStart :: Error | es) Unit
udpRecvStart recvCb handle =
  withExceptT (V.inj _udpRecvStart) $
    ExceptT $
      udpRecvStartImpl recvCb handle

foreign import udpRecvStartImpl
  :: (Maybe Buffer -> Effect Unit)
  -> UdpHandle
  -> Effect (Either Error Unit)

_udpSetBroadcast :: SProxy "udpSetBroadcast"
_udpSetBroadcast = SProxy

udpSetBroadcast
  :: ∀ es
   . Boolean
  -> UdpHandle
  -> Handler (udpSetBroadcast :: Error | es) Unit
udpSetBroadcast x handle =
  withExceptT (V.inj _udpSetBroadcast) $
    ExceptT $
      udpSetBroadcastImpl Left Right x handle

foreign import udpSetBroadcastImpl
  :: (∀ a b. a -> Either a b)
  -> (∀ a b. b -> Either a b)
  -> Boolean
  -> UdpHandle
  -> Effect (Either Error Unit)

_udpSend :: SProxy "udpSend"
_udpSend = SProxy

udpSend
  :: ∀ es sockAddr
   . IsSockAddr sockAddr
  => Array Buffer
  -> sockAddr
  -> (Either Error Unit -> Effect Unit)
  -> UdpHandle
  -> Handler (udpSend :: Error | es) Unit
udpSend bufs addr cb handle =
  withExceptT (V.inj _udpSend) $
    ExceptT $
      udpSendImpl bufs (toSockAddr addr) cb handle

foreign import udpSendImpl
  :: Array Buffer
  -> SockAddr
  -> (Either Error Unit -> Effect Unit)
  -> UdpHandle
  -> Effect (Either Error Unit)
