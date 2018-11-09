#include <uv.h>
#include <purescript.h>

#include "UV.Internal.h"
#include "UV.Timer.h"

PURS_FFI_FUNC_3(UV_Timer_timerNewImpl, _utils, _loop, _, {
	uv_loop_t *loop = purs_any_get_foreign(_loop)->data;
	uv_timer_t *handle = purs_new(uv_timer_t);
	INIT_BASE_HANDLE(handle,
			 purec_uv_timer_ctx_t,
			 purs_any_get_foreign(_utils)->data);
	return TO_EITHER(
		HANDLE_GET_UTILS(handle),
		uv_timer_init(loop, handle),
		purs_any_foreign_new(NULL, handle));
});

static void purec_uv_timer_cb(uv_timer_t* handle) {
	purec_uv_timer_ctx_t *ctx = handle->data;
	purs_any_app(ctx->cb, NULL);
}

PURS_FFI_FUNC_5(UV_Timer_timerStartImpl, _timeout, _repeat, cb, _handle, _, {
	uv_timer_t *handle = purs_any_get_foreign(_handle)->data;
	purec_uv_timer_ctx_t *ctx = handle->data;
	ctx->cb = cb;
	return TO_EITHER(
		HANDLE_GET_UTILS(handle),
		uv_timer_start(handle,
			       purec_uv_timer_cb,
			       purs_any_get_int(_timeout),
			       purs_any_get_int(_repeat)),
		NULL);
});

PURS_FFI_FUNC_2(UV_Timer_timerStopImpl, _handle, _, {
	uv_timer_t *handle = purs_any_get_foreign(_handle)->data;
	return TO_EITHER(HANDLE_GET_UTILS(handle),
			 uv_timer_stop(handle),
			 NULL);
});

PURS_FFI_FUNC_2(UV_Timer_timerAgainImpl, _handle, _, {
	uv_timer_t *handle = purs_any_get_foreign(_handle)->data;
	return TO_EITHER(HANDLE_GET_UTILS(handle),
			 uv_timer_again(handle),
			 NULL);
});

PURS_FFI_FUNC_3(UV_Timer_timerSetRepeat, _repeat, _handle, _, {
	uv_timer_t *handle = purs_any_get_foreign(_handle)->data;
	uv_timer_set_repeat(handle, purs_any_get_int(_repeat));
	return NULL;
});

PURS_FFI_FUNC_2(UV_Timer_timerGetRepeat, _handle, _, {
	uv_timer_t *handle = purs_any_get_foreign(_handle)->data;
	return purs_any_int_new(uv_timer_get_repeat(handle));
});
