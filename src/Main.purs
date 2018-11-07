module Main where

import Effect.Aff
import Prelude

import Control.Monad.Error.Class (catchError, throwError)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Symbol (reflectSymbol)
import Data.Traversable (traverse)
import Data.Variant as V
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console as Console
import UV as UV
import UV.Buffer as UV.Buffer
import Unsafe.Coerce (unsafeCoerce)

main :: Effect Unit
main = logResult =<< runExceptT do
  loop <- lift UV.defaultLoop

  lift $ testAff
  -- testUdp loop
  -- testTcp loop

  UV.run loop UV._RunDefault

  where
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

  testAff :: Effect Unit
  testAff = void $ launchAff do
    let
      serverAddr =
        UV.ip4Addr "0.0.0.0" 4321

    -- liftEffect $ Console.log "throwing..."
    -- msg1 <- (throwError unit) `catchError` \e -> do
    --   liftEffect $ Console.log "catching..."
    --   -- pure "recovered"

    -- -- liftEffect $ Console.log msg1

    let
      x :: Aff _ _
      x =
        makeAff \cb -> do
          cb (Right "Hello from Aff")
          pure (unsafeCoerce unit)
      y :: Aff _ _
      y =
        throwError unit

    -- void $ forkAff $
    --   pure unit

    void $ forkAff $
      y `catchError` \e ->
        pure unit

      -- pure "" -- unit
    -- x >>= \msg -> liftEffect $ Console.log msg
    -- map identity $
    --   x >>= \msg -> pure unit

  testTcp :: _ -> UV.Handler _ Unit
  testTcp loop = do
    let
      serverAddr =
        UV.ip4Addr "0.0.0.0" 4321

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

  testUdp :: _ -> UV.Handler _ Unit
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
