module UV.Handle
  ( class IsHandle
  , Handle
  , toHandle
  , close
  ) where

import Prelude

import Effect (Effect)
import Unsafe.Coerce (unsafeCoerce)

foreign import data Handle :: Type

class IsHandle h

toHandle :: ∀ h. IsHandle h => h -> Handle
toHandle = unsafeCoerce

close :: ∀ h. IsHandle h => Effect Unit -> h -> Effect Unit
close cb handle = closeImpl cb $ toHandle handle

foreign import closeImpl :: Effect Unit -> Handle -> Effect Unit
