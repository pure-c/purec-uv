module UV.Aff
  ( module Reexport
  , Handler
  , launchAff
  , launchAff_
  , delay
  , udpNew
  , udpBind
  , udpSend
  , udpRecvStart
  , tcpNew
  , tcpBind
  , tcpConnect
  , listen
  , readStart
  , write
  ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExceptT)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), fromRight)
import Data.Maybe (Maybe)
import Data.Variant (Variant, inj)
import Effect (Effect)
import Effect.Aff (Aff, Fiber, effectCanceler, makeAff, nonCanceler)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Console as Console
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import UV as UV
import UV hiding (Handler,udpBind,udpNew,udpSend,udpRecvStart,tcpNew,tcpBind,tcpConnect,listen,readStart,write) as Reexport

type Handler es a =
  Aff (Variant es) a

fromHandler
  :: ∀ es a
   . UV.Handler es a
  -> Handler es a
fromHandler action =
  liftEffect (runExceptT action) >>= case _ of
    Left e  -> throwError e
    Right v -> pure v

setTimeout_
  :: UV.Loop
  -> Int
  -> Effect Unit
  -> Effect Unit
setTimeout_ loop timeout cb =
  unsafePartial $ fromRight <$> do
    runExceptT $
      void $
        setTimeout loop timeout cb

setTimeout
  :: UV.Loop
  -> Int
  -> Effect Unit
  -> UV.Handler _ UV.TimerHandle
setTimeout loop timeout cb = do
  timer <- UV.timerNew loop
  timer <$ UV.timerStart (UV.Timeout timeout) (UV.Repeat 0) cb timer

delay :: Int -> UV.Loop -> Aff UV.LabeledError Unit
delay ms loop =
  makeAff \k -> do
    result <- runExceptT $
      setTimeout loop ms $
        k $ Right unit
    pure $
      effectCanceler $ case result of
        Right timer ->
          void $ runExceptT $ UV.timerStop timer
        Left e ->
          k $ Left e

launchAff_
  :: ∀ e a
   . Aff e a
  -> UV.Loop
  -> Effect Unit
launchAff_ action loop =
  void $ launchAff action loop

launchAff
  :: ∀ e a
   . Aff e a
  -> UV.Loop
  -> Effect (Fiber e a)
launchAff action loop =
  Aff.launchAff
    (setTimeout_ loop)
    (\_ -> pure $ unsafeCrashWith "TODO: render and print error")
    action

--------------------------------------------------------------------------------
-- UDP
--------------------------------------------------------------------------------

udpNew
  :: ∀ es
   . UV.Loop
  -> Handler (udpNew :: UV.Error | es) UV.UdpHandle
udpNew = fromHandler <<< UV.udpNew

udpBind
  :: ∀ es
   . UV.SockAddrIn
  -> Array UV.UdpFlag
  -> UV.UdpHandle
  -> Handler (udpBind :: UV.Error | es) Unit
udpBind addr flags handle = fromHandler $ UV.udpBind addr flags handle

udpSend
  :: ∀ es sockAddr
   . UV.IsSockAddr sockAddr
  => Array UV.Buffer
  -> sockAddr
  -> UV.UdpHandle
  -> Handler (udpSend :: UV.Error | es) Unit
udpSend bufs addr handle =
  makeAff \k ->
    -- TODO: use `uv_cancel`
    nonCanceler <$ do
      result <- runExceptT $ UV.udpSend bufs addr <@> handle $ \result' ->
        -- XXX: is "udpSend" the right label here?
        k $ lmap (inj UV._udpSend) result'
      case result of
        Right _ ->
          pure unit
        Left ve ->
          k $ Left ve

udpRecvStart
  :: ∀ es
   . (Maybe UV.Buffer -> Effect Unit)
  -> UV.UdpHandle
  -> Handler (udpRecvStart :: UV.Error | es) Unit
udpRecvStart cb handle = fromHandler $ UV.udpRecvStart cb handle

--------------------------------------------------------------------------------
-- TCP
--------------------------------------------------------------------------------

tcpNew
  :: ∀ es
   . UV.Loop
  -> Handler (tcpNew :: UV.Error | es) UV.TcpHandle
tcpNew = fromHandler <<< UV.tcpNew

tcpBind
  :: ∀ es
   . UV.SockAddrIn
  -> Array UV.TcpFlag
  -> UV.TcpHandle
  -> Handler (tcpBind :: UV.Error | es) Unit
tcpBind addr flags handle =
  fromHandler $ UV.tcpBind addr flags handle

tcpConnect
  :: ∀ es sockAddr
   . UV.IsSockAddr sockAddr
  => sockAddr
  -> UV.TcpHandle
  -> Handler (tcpConnect :: UV.Error | es) Unit
tcpConnect addr handle =
  makeAff \k ->
    -- TODO: use `uv_cancel`
    nonCanceler <$ do
      result <- runExceptT $ UV.tcpConnect addr <@> handle $ \result' ->
        -- XXX: is "tcpConnect" the right label here?
        k $ lmap (inj UV._tcpConnect) result'
      case result of
        Right _ ->
          pure unit
        Left ve ->
          k $ Left ve

--------------------------------------------------------------------------------
-- Streams
--------------------------------------------------------------------------------

listen
  :: ∀ h es
   . UV.IsStreamHandle h
  => UV.Backlog
  -> (Either UV.Error h -> Effect Unit)
  -> h
  -> Handler (listen :: UV.Error | es) Unit
listen backlog cb h =
  fromHandler $ UV.listen backlog cb h

readStart
  :: ∀ h es
   . UV.IsStreamHandle h
  => (Either UV.Error (Maybe UV.Buffer) -> Effect Unit)
  -> h
  -> Handler (readStart :: UV.Error | es) Unit
readStart cb h =
  fromHandler $ UV.readStart cb h

write
  :: ∀ h es
   . UV.IsStreamHandle h
  => Array UV.Buffer
  -> h
  -> Handler (write :: UV.Error | es) Unit
write bufs handle =
  makeAff \k ->
    -- TODO: use `uv_cancel`
    nonCanceler <$ do
      result <- runExceptT $ UV.write bufs <@> handle $ \result' ->
        -- XXX: is "write" the right label here?
        k $ lmap (inj UV._write) result'
      case result of
        Right _ ->
          pure unit
        Left ve ->
          k $ Left ve
