module Main where

import Prelude

import Control.Monad.Except (runExceptT)
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

  recvH <- UV.udpNew loop
  UV.udpBind (UV.ip4Addr "0.0.0.0" 1234) [ UV._UdpReuseAddr ] recvH
  UV.udpRecvStart <@> recvH $ \(mBuf :: Maybe UV.Buffer) -> do
    mS <- traverse UV.bufferToString mBuf
    Console.log $ "Received: " <> show mS

  sendH <- UV.udpNew loop
  UV.udpBind (UV.ip4Addr "0.0.0.0" 1235) [ UV._UdpReuseAddr ] sendH
  UV.udpSetBroadcast true sendH

  buf <- lift $ UV.bufferFromString "hello"
  UV.udpSend [ buf ] (UV.ip4Addr "0.0.0.0" 1234)
    (case _ of
      Right _ ->
        Console.log "sent"
      Left errCode ->
        Console.log $ renderErrCode errCode
    ) sendH

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
