module UV.Loop
  ( Loop
  , RunMode
  , LoopOption
  , _RunDefault
  , _RunOnce
  , _RunNoWait
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
import UV.Internal as UV
import UV.Internal as UV.Internal

foreign import data RunMode :: Type
foreign import _RunDefault :: RunMode
foreign import _RunOnce :: RunMode
foreign import _RunNoWait :: RunMode

foreign import data Loop :: Type
foreign import data LoopOption :: Type

newLoop :: Effect Loop
newLoop = newLoopImpl UV.Internal.utils

foreign import newLoopImpl :: UV.Utils -> Effect Loop

_run :: SProxy "run"
_run = SProxy

run
  :: ∀ es
   . RunMode
  -> Loop
  -> Handler (run :: Error | es) Unit
run mode loop = mkHandler _run $ runImpl mode loop

foreign import runImpl
  :: RunMode
  -> Loop
  -> Effect (Either Error Unit)
