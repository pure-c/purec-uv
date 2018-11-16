module Test.Main where

import Prelude

import Control.Monad.Cont (lift)
import Control.Monad.Except (runExceptT)
import Data.Either (Either(..), either)
import Data.Symbol (reflectSymbol)
import Data.Variant as V
import Effect (Effect)
import Effect.Console as Console
import UV as UV

foreign import exit :: ∀ a. Int -> Effect a

main :: Effect Unit
main = void $ either die pure =<< do
  runExceptT do
    loop <- lift UV.newLoop
    UV.timerNew loop >>= do
      UV.timerStart (0 # UV.Timeout) (0 # UV.Repeat) do
        Console.log "Hello from UV!"
    UV.run UV._RunDefault loop

  where
  die ve =
    let
      errLabel = rowLabel ve
      errCode = UV.errCode ve
    in do
      Console.log $ "Failure(" <> errLabel <> "): " <> renderErrCode errCode
      exit 1

  renderErrCode code =
    UV.strerror code <> " (" <> show code <> ")"

  rowLabel v =
    V.unvariant v # \(V.Unvariant k) ->
      k \sym _ -> reflectSymbol sym
