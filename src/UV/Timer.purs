module UV.Timer
  ( TimerHandle
  , Timeout(Timeout)
  , Repeat(Repeat)
  , _timerNew
  , timerNew
  , _timerStart
  , timerStart
  , _timerStop
  , timerStop
  , timerSetRepeat
  , timerGetRepeat
  , _timerAgain
  , timerAgain
  ) where

import Prelude

import Control.Monad.Except (ExceptT(..), withExceptT)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Variant as V
import Effect (Effect)
import UV.Error (Error)
import UV.Internal as UV
import UV.Internal as UV.Internal
import UV.Loop as UV
import UV.Types (Handler, mkHandler)

newtype Timeout = Timeout Int
newtype Repeat = Repeat Int

foreign import data TimerHandle :: Type

_timerNew :: SProxy "timerNew"
_timerNew = SProxy

timerNew
  :: ∀ es
   . UV.Loop
  -> Handler (timerNew :: Error | es) TimerHandle
timerNew =
  let go = timerNewImpl UV.Internal.utils
   in mkHandler _timerNew <<< go

foreign import timerNewImpl
  :: UV.Utils
  -> UV.Loop
  -> Effect (Either Error TimerHandle)

_timerStart :: SProxy "timerStart"
_timerStart = SProxy

timerStart
  :: ∀ es
   . Timeout
  -> Repeat
  -> Effect Unit
  -> TimerHandle
  -> Handler (timerStart :: Error | es) Unit
timerStart timeout repeat cb handle =
  mkHandler _timerStart $
    timerStartImpl timeout repeat cb handle

foreign import timerStartImpl
  :: Timeout
  -> Repeat
  -> Effect Unit
  -> TimerHandle
  -> Effect (Either Error Unit)

_timerStop :: SProxy "timerStop"
_timerStop = SProxy

timerStop
  :: ∀ es
   . TimerHandle
  -> Handler (timerStop :: Error | es) Unit
timerStop handle =
  mkHandler _timerStop $
    timerStopImpl handle

foreign import timerStopImpl
  :: TimerHandle
  -> Effect (Either Error Unit)

_timerAgain :: SProxy "timerAgain"
_timerAgain = SProxy

timerAgain
  :: ∀ es
   . TimerHandle
  -> Handler (timerAgain :: Error | es) Unit
timerAgain handle =
  mkHandler _timerAgain $
    timerAgainImpl handle

foreign import timerAgainImpl
  :: TimerHandle
  -> Effect (Either Error Unit)

foreign import timerSetRepeat
  :: Repeat
  -> TimerHandle
  -> Effect (Either Error Unit)

foreign import timerGetRepeat
  :: TimerHandle
  -> Effect Int
