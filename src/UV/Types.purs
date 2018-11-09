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
import Data.Either (Either)
import Data.Symbol (class IsSymbol, SProxy)
import Data.Variant (Variant, inj)
import Effect (Effect)
import Prim.Row (class Cons)
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

mkHandler
  :: ∀ ls e l a f es
   . Functor f
  => Cons l e ls es
  => IsSymbol l
  => SProxy l
  -> f (Either e a)
  -> ExceptT (Variant es) f a
mkHandler sproxy x =
  withExceptT (inj sproxy) $
    ExceptT x
