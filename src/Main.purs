module Main where

import Prelude

import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Maybe (Maybe)
import Data.Symbol (reflectSymbol)
import Data.Traversable (traverse)
import Data.Variant as V
import Effect (Effect)
import Effect.Console as Console
import UV as UV

main :: Effect Unit
main = logResult =<< runExceptT do
  loop <- lift UV.defaultLoop

  testUdp loop
  testTcp loop

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
            lift $ Console.log $ "tcp server: listen success"
            UV.readStart <@> clientH $ \_ ->
              Console.log "tcp server: read something"

    clientH <- UV.tcpNew loop
    UV.tcpConnect serverAddr <@> clientH $ \result -> do
      case result of
        Left err ->
          Console.log $ "tcp connect: failure: " <> renderErrCode err
        Right _ ->
          Console.log "tcp connect: success"

    pure unit -- To be continued ...

  testUdp :: _ -> UV.Handler _ Unit
  testUdp loop = do
    recvH <- UV.udpNew loop
    UV.udpBind (UV.ip4Addr "0.0.0.0" 1234) [ UV._UdpReuseAddr ] recvH
    UV.udpRecvStart <@> recvH $ \(mBuf :: Maybe UV.Buffer) -> do
      mS <- traverse UV.bufferToString mBuf
      Console.log $ "udp: received: " <> show mS

    sendH <- UV.udpNew loop

    buf <- lift $ UV.bufferFromString "hello"
    UV.udpSend [ buf ] (UV.ip4Addr "0.0.0.0" 1234)
      (case _ of
        Right _ ->
          Console.log "udp: sent"
        Left errCode ->
          Console.log $ renderErrCode errCode
      ) sendH
