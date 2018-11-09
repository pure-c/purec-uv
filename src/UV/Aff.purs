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
import UV hiding (Handler,udpBind,udpNew,udpSend,udpRecvStart) as Reexport

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
   . UV.Loop
  -> Aff e a
  -> Effect Unit
launchAff_ loop action = void $ launchAff loop action

launchAff
  :: ∀ e a
   . UV.Loop
  -> Aff e a
  -> Effect (Fiber e a)
launchAff loop =
  Aff.launchAff
    (setTimeout_ loop)
    (\_ -> pure $ unsafeCrashWith "TODO: render and print error")

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
  makeAff \k -> do
    Console.log "few taps"
    -- TODO: use `uv_cancel`
    nonCanceler <$ do
      result <- runExceptT $ UV.udpSend bufs addr <@> handle $ \result' -> do
        Console.log "klara"
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
