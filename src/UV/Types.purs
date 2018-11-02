module UV.Types
  ( Handler
  , mkHandler
  , Ip
  , Port
  , class IsSockAddr
  , toSockAddr
  , SockAddrIn
  , SockAddr
  ) where

import Prelude

import Control.Monad.Except (ExceptT(..), withExceptT)
import Data.Variant (Variant, inj)
import Effect (Effect)
import Unsafe.Coerce (unsafeCoerce)

foreign import data SockAddrIn :: Type
foreign import data SockAddr :: Type

class IsSockAddr a where
  toSockAddr :: a -> SockAddr

instance isSockAddrSockAddrIn :: IsSockAddr SockAddrIn where
  toSockAddr = unsafeCoerce

type Ip = String
type Port = Int

type Handler es a =
  ExceptT (Variant es) Effect a

mkHandler sproxy x =
  withExceptT (inj sproxy) $
    ExceptT x
