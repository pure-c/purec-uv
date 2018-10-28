#include <uv.h>
#include <purescript.h>

PURS_FFI_VALUE(UV__RunDefault, PURS_ANY_INT(UV_RUN_DEFAULT));
PURS_FFI_VALUE(UV__RunOnce, PURS_ANY_INT(UV_RUN_ONCE));
PURS_FFI_VALUE(UV__RunNoWait, PURS_ANY_INT(UV_RUN_NOWAIT));

PURS_ANY_THUNK_DEF(
	UV_defaultLoop,
	purs_any_foreign_new(NULL, (void*) uv_default_loop()));

PURS_FFI_FUNC_1(UV_defaultLoop, _, {
	return UV_defaultLoop;
});

PURS_FFI_FUNC_1(UV_newLoop, _, {
	return purs_any_foreign_new(NULL, (void*) uv_loop_new());
});

PURS_FFI_FUNC_5(UV_runImpl, Left, Right, _loop, _mode, _, {
	uv_loop_t* loop = purs_any_get_foreign(_loop)->data;
	uv_run_mode mode = purs_any_get_int(_mode);
	int ret = uv_run(loop, mode);
	return ret == UV_OK
		? purs_any_app(Right, NULL)
		: purs_any_app(Left, purs_any_int_new(ret));
});
