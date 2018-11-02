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

import UV.Stream (class IsStreamHandle)
import UV.Types (class IsSockAddr, Handler, SockAddr, SockAddrIn, toSockAddr)
import Control.Monad.Except (ExceptT(..), withExceptT)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Variant as V
import Effect (Effect)
import UV.Error (Error)
import UV.Loop (Loop)
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
