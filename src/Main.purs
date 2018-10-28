module Main where

import Prelude

import Control.Monad.Except (runExceptT)
import Control.Monad.Trans.Class (lift)
import Effect (Effect)
import UV as UV

main :: Effect Unit
main = void $ runExceptT do
  loop <- lift UV.defaultLoop
  UV.run loop UV._RunDefault
