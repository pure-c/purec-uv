module UV.Error
  ( Error
  , LabeledError
  , errCode
  , strerror
  ) where

import Prelude

import Data.Variant (Variant)
import Data.Variant as V
import Unsafe.Coerce (unsafeCoerce)

type LabeledError =
  Variant
    ( run :: Error
    , tcpNew :: Error
    , tcpBind :: Error
    , tcpConnect :: Error
    , udpNew :: Error
    , udpBind :: Error
    , udpRecvStart :: Error
    , udpSetBroadcast :: Error
    , udpSend :: Error
    , listen :: Error
    , readStart :: Error
    , write :: Error
    , timerNew :: Error
    , timerStart :: Error
    , timerStop :: Error
    , timerAgain :: Error
    , timerSetRepeat :: Error
    )

foreign import data Error :: Type

instance showError :: Show Error where
  show x = show (unsafeCoerce x :: Int)

errCode
  :: LabeledError
  -> Error
errCode e =
  V.unvariant e # \(V.Unvariant k) ->
    k \_ v ->
      unsafeCoerce v

foreign import strerror :: Error -> String
