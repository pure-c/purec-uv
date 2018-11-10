module Main where

import Effect.Aff
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
import Effect.Class (liftEffect)
import Effect.Console as Console
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import UV.Aff as UV
import UV.Buffer as UV.Buffer

main :: Effect Unit
main = do
  loop <- UV.newLoop
  UV.launchAff_ <@> loop $ do
    testUdp loop `catchError` \ve ->
      liftEffect $ Console.log $
        "testUdp: failure(" <> rowLabel ve <> "): " <>
          renderErrCode (UV.errCode ve)
    testTcp loop `catchError` \ve ->
      liftEffect $ Console.log $
        "testTcp: failure(" <> rowLabel ve <> "): " <>
          renderErrCode (UV.errCode ve)
  runExceptT (UV.run UV._RunDefault loop) >>= case _ of
    Left ve ->
      Console.log $
        "Failure(" <> rowLabel ve <> "): " <> renderErrCode (UV.errCode ve)
    Right _ ->
      Console.log "Done!"

  where
  testUdp :: UV.Loop -> UV.Handler _ Unit
  testUdp loop = do
    recvH <- UV.udpNew loop
    UV.udpBind (UV.ip4Addr "0.0.0.0" 1234) [ UV._UdpReuseAddr ] recvH
    UV.udpRecvStart <@> recvH $ \mBuf -> do
      mS <- traverse UV.Buffer.toString mBuf
      Console.log $ "udp: received: " <> show mS
    sendH <- UV.udpNew loop
    buf   <- liftEffect $ UV.Buffer.fromString "udp: hello"
    UV.udpSend [ buf ] (UV.ip4Addr "0.0.0.0" 1234) sendH
    UV.close recvH
    UV.close sendH

  renderErrCode errCode =
    UV.strerror errCode <> " (" <> show errCode <> ")"

  rowLabel v =
    V.unvariant v # \(V.Unvariant k) ->
      k \sym _ ->
        reflectSymbol sym

  testTcp :: _ -> UV.Handler _ Unit
  testTcp loop = do
    let
      serverAddr =
        UV.ip4Addr "0.0.0.0" 4321

    serverH <- UV.tcpNew loop
    UV.tcpBind serverAddr [] serverH

    UV.listen (UV.Backlog 128) <@> serverH $ \result ->
      UV.launchAff_ <@> loop $
        case result of
          Left err ->
            liftEffect $
              Console.log $ "tcp server: listen failure: " <> renderErrCode err
          Right clientH -> do
            liftEffect $
              Console.log $ "tcp: server: listen success"
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
    UV.tcpConnect serverAddr clientH

    buf <- liftEffect $ UV.Buffer.fromString "tcp: hello"
    UV.write [ buf ] clientH
    UV.close clientH
    UV.close serverH
