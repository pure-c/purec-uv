module UV.Tcp
  ( TcpHandle
  , TcpFlag
  , _tcpNew
  , tcpNew
  , _tcpBind
  , tcpBind
  , _tcpConnect
  , tcpConnect
  ) where

import Prelude

import Control.Monad.Except (ExceptT(..), withExceptT)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Variant as V
import Effect (Effect)
import UV.Error (Error)
import UV.Internal as UV
import UV.Internal as UV.Internal
import UV.Loop as UV
import UV.Stream (class IsStreamHandle)
import UV.Types (class IsSockAddr, Handler, SockAddr, SockAddrIn, mkHandler, toSockAddr)
import Unsafe.Coerce (unsafeCoerce)

foreign import data TcpHandle :: Type
foreign import data TcpFlag :: Type

instance isStreamHandleTcpHandle :: IsStreamHandle TcpHandle where
  toStreamHandle = unsafeCoerce

foreign import _TcpIpv6only :: TcpFlag

_tcpNew :: SProxy "tcpNew"
_tcpNew = SProxy

tcpNew
  :: ∀ es
   . UV.Loop
  -> Handler (tcpNew :: Error | es) TcpHandle
tcpNew =
  let go = tcpNewImpl UV.Internal.utils
   in mkHandler _tcpNew <<< go

foreign import tcpNewImpl
  :: UV.Utils
  -> UV.Loop
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
  mkHandler _tcpBind $
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
  mkHandler _tcpConnect $
    tcpConnectImpl (toSockAddr addr) cb handle

foreign import tcpConnectImpl
  :: SockAddr
  -> (Either Error Unit -> Effect Unit)
  -> TcpHandle
  -> Effect (Either Error Unit)
