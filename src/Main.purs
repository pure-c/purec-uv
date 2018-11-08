module Main where

import Prelude

import Control.Monad.Error.Class (catchError, throwError)
import Control.Monad.Except (mapExceptT, runExceptT)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..), either, fromRight)
import Data.Maybe (Maybe(..))
import Data.Symbol (reflectSymbol)
import Data.Traversable (traverse)
import Data.Variant as V
import Effect (Effect)
import Effect.Aff (Aff, effectCanceler, launchAff, makeAff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Partial.Unsafe (unsafePartial)
import UV as UV
import UV.Buffer as UV.Buffer

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

delay :: UV.Loop -> Int -> Aff UV.LabeledError Unit
delay loop ms =
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

main :: Effect Unit
main = logResult =<< runExceptT do
  loop <- lift UV.defaultLoop

  lift $ testAff loop
  -- testUdp loop
  -- testTcp loop

  UV.run loop UV._RunDefault

  where
  testAff :: UV.Loop -> Effect Unit
  testAff loop = void $ launchAff (setTimeout_ loop) do
    let
      serverAddr =
        UV.ip4Addr "0.0.0.0" 80
      go =
        either throwError pure =<< do
          runExceptT $
            mapExceptT liftEffect do
              serverH <- UV.tcpNew loop
              UV.tcpBind serverAddr [] serverH

    liftEffect $ Console.log "wait..."
    delay loop 1000
    liftEffect $ Console.log "waited enough!"

    go `catchError` \ve ->
      liftEffect $
        Console.log $
          rowLabel ve <> ": " <> UV.strerror (UV.errCode ve)

  testTcp :: _ -> UV.Handler _ Unit
  testTcp loop = do
    let
      serverAddr =
        UV.ip4Addr "0.0.0.0" 80 -- 4321

    serverH <- UV.tcpNew loop
    UV.tcpBind serverAddr [] serverH
    UV.listen (UV.Backlog 128) <@> serverH $ \result -> do
      logResult =<< runExceptT do
        case result of
          Left err ->
            lift $ Console.log $ "tcp server: listen failure: " <> renderErrCode err
          Right clientH -> do
            lift $ Console.log $ "tcp: server: listen success"
            UV.readStart <@> clientH $ \result ->
              case result of
                Left err ->
                  Console.log $ "tcp: read: failure: " <> renderErrCode err
                Right Nothing ->
                  Console.log "tcp: read: EOF"
                Right (Just buf) -> do
                  s <- UV.Buffer.toString buf
                  Console.log $ "tcp: read: " <> s

    clientH <- UV.tcpNew loop
    UV.tcpConnect serverAddr <@> clientH $ \result -> do
      case result of
        Left err ->
          Console.log $ "tcp: connect: failure: " <> renderErrCode err
        Right _ ->
          Console.log "tcp: connect: success"

    buf <- lift $ UV.Buffer.fromString "tcp: hello"
    UV.write [ buf ] <@> clientH $ \result -> do
      case result of
        Left err ->
          Console.log $ "tcp: write: failure: " <> renderErrCode err
        Right _ ->
          Console.log "tcp: write: success"

    pure unit -- To be continued ...

  testUdp :: UV.Loop -> UV.Handler _ Unit
  testUdp loop = do
    recvH <- UV.udpNew loop
    UV.udpBind (UV.ip4Addr "0.0.0.0" 1234) [ UV._UdpReuseAddr ] recvH
    UV.udpRecvStart <@> recvH $ \mBuf -> do
      mS <- traverse UV.Buffer.toString mBuf
      Console.log $ "udp: received: " <> show mS

    sendH <- UV.udpNew loop

    buf <- lift $ UV.Buffer.fromString "udp: hello"
    UV.udpSend [ buf ] (UV.ip4Addr "0.0.0.0" 1234)
      (case _ of
        Right _ ->
          Console.log "udp: sent"
        Left errCode ->
          Console.log $ renderErrCode errCode
      ) sendH

  logResult (Left ve) =
    let
      errLabel = rowLabel ve
      errCode = UV.errCode ve
    in
      Console.log $
        "Failure(" <> errLabel <> "): " <> renderErrCode errCode
  logResult (Right _) =
    Console.log "Done!"

  renderErrCode errCode =
    UV.strerror errCode <> " (" <> show errCode <> ")"

  rowLabel v =
    V.unvariant v # \(V.Unvariant k) ->
      k \sym _ ->
        reflectSymbol sym
