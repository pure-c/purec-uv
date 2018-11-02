#include <uv.h>
#include <purescript.h>
#include "UV.Internal.h"

PURS_FFI_VALUE(UV_Loop__RunDefault, PURS_ANY_INT(UV_RUN_DEFAULT));
PURS_FFI_VALUE(UV_Loop__RunOnce, PURS_ANY_INT(UV_RUN_ONCE));
PURS_FFI_VALUE(UV_Loop__RunNoWait, PURS_ANY_INT(UV_RUN_NOWAIT));

PURS_ANY_THUNK_DEF(
	thunked_defaultLoop,
	purs_any_foreign_new(NULL, (void*) uv_default_loop()));

PURS_FFI_FUNC_1(UV_Loop_defaultLoop, _, {
	return thunked_defaultLoop;
});

PURS_FFI_FUNC_1(UV_Loop_newLoop, _, {
	return purs_any_foreign_new(NULL, (void*) uv_loop_new());
});

PURS_FFI_FUNC_5(UV_Loop_runImpl, Left, Right, _loop, _mode, _, {
	uv_loop_t* loop = purs_any_get_foreign(_loop)->data;
	uv_run_mode mode = purs_any_get_int(_mode);
	return TO_EITHER(uv_run(loop, mode), NULL);
});
