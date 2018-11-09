module UV.Udp
  ( UdpHandle
  , UdpFlag
  , _UdpIpv6only
  , _UdpPartial
  , _UdpReuseAddr
  , _udpNew
  , udpNew
  , _udpBind
  , udpBind
  , _udpRecvStart
  , udpRecvStart
  , _udpSetBroadcast
  , udpSetBroadcast
  , _udpSend
  , udpSend
  ) where

import Prelude

import Control.Monad.Except (ExceptT(..), withExceptT)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Variant as V
import Effect (Effect)
import UV.Buffer (Buffer)
import UV.Error (Error)
import UV.Internal as UV
import UV.Internal as UV.Internal
import UV.Loop as UV
import UV.Types (class IsSockAddr, Handler, SockAddr, SockAddrIn, mkHandler, toSockAddr)

foreign import data UdpHandle :: Type
foreign import data UdpFlag :: Type

foreign import _UdpIpv6only :: UdpFlag
foreign import _UdpPartial :: UdpFlag
foreign import _UdpReuseAddr :: UdpFlag

_udpNew :: SProxy "udpNew"
_udpNew = SProxy

udpNew
  :: ∀ es
   . UV.Loop
  -> Handler (udpNew :: Error | es) UdpHandle
udpNew =
  let go = udpNewImpl UV.Internal.utils
   in mkHandler _udpNew <<< go

foreign import udpNewImpl
  :: UV.Utils
  -> UV.Loop
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
  mkHandler _udpBind $
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
  mkHandler _udpRecvStart $
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
  mkHandler _udpSetBroadcast $
    udpSetBroadcastImpl x handle

foreign import udpSetBroadcastImpl
  :: Boolean
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
  mkHandler _udpSend $
    udpSendImpl bufs (toSockAddr addr) cb handle

foreign import udpSendImpl
  :: Array Buffer
  -> SockAddr
  -> (Either Error Unit -> Effect Unit)
  -> UdpHandle
  -> Effect (Either Error Unit)
