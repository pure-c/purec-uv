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
import Effect (Effect)
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)

type Error = Int

foreign import strerror :: Error -> String

foreign import data RunMode :: Type
foreign import _RunDefault :: RunMode
foreign import _RunOnce :: RunMode
foreign import _RunNoWait :: RunMode

foreign import data Loop :: Type
foreign import defaultLoop :: Effect Loop
foreign import newLoop :: Effect Loop
foreign import data LoopOption :: Type

foreign import runImpl
  :: (∀ a b. a -> Either a b)
  -> (∀ a b. b -> Either a b)
  -> Loop
  -> RunMode
  -> Effect (Either Error Unit)

run :: Loop -> RunMode -> ExceptT Error Effect Unit
run loop mode = ExceptT $ runImpl Left Right loop mode

--------------------------------------------------------------------------------
-- Streams
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

foreign import listenImpl
  :: (∀ a b. a -> Either a b)
  -> (∀ a b. b -> Either a b)
  -> Stream
  -> Backlog
  -> (Unit -> Effect Unit)
  -> Effect (Either Error Unit)

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

--------------------------------------------------------------------------------
-- Networking
--------------------------------------------------------------------------------

foreign import data SockAddrIn :: Type

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

foreign import udpNewImpl
  :: (∀ a b. a -> Either a b)
  -> (∀ a b. b -> Either a b)
  -> Loop
  -> Effect (Either Error UdpHandle)

udpNew :: Loop -> ExceptT Error Effect UdpHandle
udpNew loop = ExceptT $ udpNewImpl Left Right loop

foreign import udpBindImpl
  :: (∀ a b. a -> Either a b)
  -> (∀ a b. b -> Either a b)
  -> SockAddrIn
  -> Array UdpFlag
  -> UdpHandle
  -> Effect (Either Error Unit)

udpBind :: SockAddrIn -> Array UdpFlag -> UdpHandle -> ExceptT Error Effect Unit
udpBind addr flags handle = ExceptT $ udpBindImpl Left Right addr flags handle

foreign import udpRecvStartImpl
  :: (∀ a b. a -> Either a b)
  -> (∀ a b. b -> Either a b)
  -> (Unit -> Effect Unit)
  -> UdpHandle
  -> Effect (Either Error Unit)

udpRecvStart
  :: (Unit -> Effect Unit)
  -> UdpHandle
  -> ExceptT Error Effect Unit
udpRecvStart recvCb handle =
  ExceptT $ udpRecvStartImpl Left Right recvCb handle
