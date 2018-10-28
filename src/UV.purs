module UV where

import Prelude

import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (ExceptT(..))
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Effect (Effect)

foreign import data ErrNo :: Type

foreign import data RunMode :: Type
foreign import _RunDefault :: RunMode
foreign import _RunOnce :: RunMode
foreign import _RunNoWait :: RunMode

foreign import data Loop :: Type
foreign import defaultLoop :: Effect Loop
foreign import newLoop :: Effect Loop
foreign import data LoopOption :: Type

foreign import runImpl
  :: (∀ a b. a -> Either a b)
  -> (∀ a b. b -> Either a b)
  -> Loop
  -> RunMode
  -> Effect (Either ErrNo Unit)

run :: Loop -> RunMode -> ExceptT ErrNo Effect Unit
run loop mode = ExceptT $ runImpl Left Right loop mode
