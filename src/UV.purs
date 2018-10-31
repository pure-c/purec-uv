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
      , udpNew :: Error
      , udpBind :: Error
      , udpRecvStart :: Error
      , udpSetBroadcast :: Error
      , udpSend :: Error
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

foreign import data Stream :: Type

class CanListen h

class IsStream h where
  toStream :: h -> Stream

instance udpIsStream :: IsStream UdpHandle where
  toStream = unsafeCoerce

newtype Backlog =
  Backlog
    Int

listen
  :: ∀ h
   . IsStream h
  => CanListen h
  => h
  -> Backlog
  -> (Unit -> Effect Unit)
  -> ExceptT Error Effect Unit
listen h backlog cb =
  ExceptT $ listenImpl Left Right (toStream h) backlog cb

foreign import listenImpl
  :: (∀ a b. a -> Either a b)
  -> (∀ a b. b -> Either a b)
  -> Stream
  -> Backlog
  -> (Unit -> Effect Unit)
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
      udpBindImpl Left Right addr flags handle

foreign import udpBindImpl
  :: (∀ a b. a -> Either a b)
  -> (∀ a b. b -> Either a b)
  -> SockAddrIn
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
