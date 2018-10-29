module Main where

import Prelude

import Control.Monad.Except (runExceptT)
import Control.Monad.Trans.Class (lift)
import Effect (Effect)
import UV as UV

main :: Effect Unit
main = void $ runExceptT do
  loop   <- lift UV.defaultLoop
  server <- UV.udpInit loop
  UV.udpBind server $ UV.ip4Addr "0.0.0.0" 1234
  -- int r = uv_listen((uv_stream_t*) &server, DEFAULT_BACKLOG, on_new_connection);
  -- if (r) {
  --     fprintf(stderr, "Listen error %s\n", uv_strerror(r));
  --     return 1;
  -- }
  UV.run loop UV._RunDefault
