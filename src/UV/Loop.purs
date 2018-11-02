module UV.Loop
  ( Loop
  , RunMode
  , LoopOption
  , _RunDefault
  , _RunOnce
  , _RunNoWait
  , defaultLoop
  , newLoop
  , _run
  , run
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Symbol (SProxy(..))
import Effect (Effect)
import UV.Types (Handler, mkHandler)
import UV.Error (Error)

foreign import data RunMode :: Type
foreign import _RunDefault :: RunMode
foreign import _RunOnce :: RunMode
foreign import _RunNoWait :: RunMode

foreign import data Loop :: Type
foreign import defaultLoop :: Effect Loop
foreign import newLoop :: Effect Loop
foreign import data LoopOption :: Type

_run :: SProxy "run"
_run = SProxy

run
  :: ∀ es
   . Loop
  -> RunMode
  -> Handler (run :: Error | es) Unit
run loop mode =
  mkHandler _run $
    runImpl Left Right loop mode

foreign import runImpl
  :: (∀ a b. a -> Either a b)
  -> (∀ a b. b -> Either a b)
  -> Loop
  -> RunMode
  -> Effect (Either Error Unit)
