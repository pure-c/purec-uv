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
  h    <- UV.udpNew loop
  UV.udpBind (UV.ip4Addr "0.0.0.0" 1234) [ UV._UdpReuseAddr ] h
  UV.udpRecvStart <@> h $ \_ ->
    Console.log $ "received: affen" <> a
  UV.run loop UV._RunDefault

  where
  logResult (Left n) =
    Console.log $ "Failure(" <> show n <> "): " <> UV.strerror n
  logResult (Right _) =
    Console.log "Done!"
