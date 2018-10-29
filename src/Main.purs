module Main where

import Prelude

import Control.Monad.Except (runExceptT)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Console as Console
import UV as UV

main :: Effect Unit
main = logResult =<< runExceptT do
  let
    a =
      "stall"
  loop <- lift UV.defaultLoop

  recvH <- UV.udpNew loop
  UV.udpBind (UV.ip4Addr "0.0.0.0" 1234) [ UV._UdpReuseAddr ] recvH
  UV.udpRecvStart <@> recvH $ \_ ->
    Console.log $ "received: affen" <> a

  sendH <- UV.udpNew loop
  UV.udpSetBroadcast true sendH


  UV.run loop UV._RunDefault

  where
  logResult (Left e) =
    let
      errCode =
        UV.errCode e
    in
      Console.log $
        "Failure(" <> show errCode <> "): " <> UV.strerror errCode
  logResult (Right _) =
    Console.log "Done!"
