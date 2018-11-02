module UV.Common
  ( ip4Addr
  ) where

import UV.Types (Ip, Port, SockAddrIn)

foreign import ip4Addr :: Ip -> Port -> SockAddrIn
