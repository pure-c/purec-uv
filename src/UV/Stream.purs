module UV.Stream
  ( StreamHandle
  , Backlog(..)
  , class IsStreamHandle
  , toStreamHandle
  , _listen
  , listen
  , _readStart
  , readStart
  , _write
  , write
  ) where

import Prelude

import Control.Monad.Except (ExceptT(..), withExceptT)
import Data.Either (Either)
import Data.Maybe (Maybe)
import Data.Symbol (SProxy(..))
import Data.Variant as V
import Effect (Effect)
import UV.Buffer (Buffer)
import UV.Error (Error)
import UV.Types (Handler, mkHandler)

foreign import data StreamHandle :: Type

class IsStreamHandle h where
  toStreamHandle :: h -> StreamHandle

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
  mkHandler _listen $
    listenImpl backlog cb $ toStreamHandle h

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
  => (Either Error (Maybe Buffer) -> Effect Unit)
  -> h
  -> Handler (readStart :: Error | es) Unit
readStart cb h =
  mkHandler _readStart $
    readStartImpl cb $ toStreamHandle h

foreign import readStartImpl
  :: (Either Error (Maybe Buffer) -> Effect Unit)
  -> StreamHandle
  -> Effect (Either Error Unit)

_write :: SProxy "write"
_write = SProxy

write
  :: ∀ h es
   . IsStreamHandle h
  => Array Buffer
  -> (Either Error Unit -> Effect Unit)
  -> h
  -> Handler (write :: Error | es) Unit
write bufs cb h =
  mkHandler _write $
    writeImpl bufs cb $ toStreamHandle h

foreign import writeImpl
  :: Array Buffer
  -> (Either Error Unit -> Effect Unit)
  -> StreamHandle
  -> Effect (Either Error Unit)
