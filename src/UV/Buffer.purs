module UV.Buffer
  ( Buffer
  , fromString
  , toString
  ) where

import Effect (Effect)

foreign import data Buffer :: Type

foreign import fromString :: String -> Effect Buffer
foreign import toString :: Buffer -> Effect String
