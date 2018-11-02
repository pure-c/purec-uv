module UV.Internal where

import Data.Unit (Unit)

-- XXX this is required in order for the FFI file to be copied over which
--     contains ancillary macros and functions used by other modules.
foreign import dummy :: Unit
