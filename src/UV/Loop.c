#include <uv.h>
#include <purescript.h>
#include "UV.Internal.h"

PURS_FFI_VALUE(UV_Loop__RunDefault, PURS_ANY_INT(UV_RUN_DEFAULT));
PURS_FFI_VALUE(UV_Loop__RunOnce, PURS_ANY_INT(UV_RUN_ONCE));
PURS_FFI_VALUE(UV_Loop__RunNoWait, PURS_ANY_INT(UV_RUN_NOWAIT));

PURS_FFI_FUNC_2(UV_Loop_newLoopImpl, _utils, _, {
	uv_loop_t* loop = uv_loop_new();
	loop->data = purs_any_get_foreign(_utils)->data;
	purec_uv_utils_t * utils = loop->data;
	return purs_any_foreign_new(NULL, loop);
});

PURS_FFI_FUNC_3(UV_Loop_runImpl, _mode, _loop, _, {
	uv_loop_t* loop = purs_any_get_foreign(_loop)->data;
	uv_run_mode mode = purs_any_get_int(_mode);
	purec_uv_utils_t * utils = loop->data;
	return TO_EITHER(utils, uv_run(loop, mode), NULL);
});
