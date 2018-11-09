module UV.Internal where

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Unit (Unit)

foreign import data Utils :: Type

utils :: Utils
utils = mkUtils Left Right Nothing Just

foreign import mkUtils
  :: (∀ a b. a -> Either a b)
  -> (∀ a b. b -> Either a b)
  -> (∀ a. Maybe a)
  -> (∀ a. a -> Maybe a)
  -> Utils
