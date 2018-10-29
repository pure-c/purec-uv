module UV where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (ExceptT(..))
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Effect (Effect)

foreign import data ErrNo :: Type

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
  -> Effect (Either ErrNo Unit)

run :: Loop -> RunMode -> ExceptT ErrNo Effect Unit
run loop mode = ExceptT $ runImpl Left Right loop mode

foreign import data SockAddrIn :: Type

type Ip = String
type Port = Int

foreign import ip4Addr :: Ip -> Port -> SockAddrIn

--------------------------------------------------------------------------------
-- UDP
--------------------------------------------------------------------------------

foreign import data Udp :: Type

foreign import udpInitImpl
  :: (∀ a b. a -> Either a b)
  -> (∀ a b. b -> Either a b)
  -> Loop
  -> Effect (Either ErrNo Udp)

udpInit :: Loop -> ExceptT ErrNo Effect Udp
udpInit loop = ExceptT $ udpInitImpl Left Right loop

foreign import udpBindImpl
  :: (∀ a b. a -> Either a b)
  -> (∀ a b. b -> Either a b)
  -> Udp
  -> SockAddrIn
  -> Effect (Either ErrNo Unit)

udpBind :: Udp -> SockAddrIn -> ExceptT ErrNo Effect Unit
udpBind udp addr = ExceptT $ udpBindImpl Left Right udp addr
