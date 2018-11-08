#ifndef PUREC_UV_TIMER_H
#define PUREC_UV_TIMER_H

#include <uv.h>
#include <purescript.h>

#include "UV.Internal.h"

typedef struct purec_uv_timer_ctx_s {
	BASE_HANDLE_FIELDS
	const purs_any_t* cb;
} purec_uv_timer_ctx_t;

PURS_FFI_EXPORT(UV_Timer_timerNewImpl);
PURS_FFI_EXPORT(UV_Timer_timerStartImpl);
PURS_FFI_EXPORT(UV_Timer_timerStopImpl);
PURS_FFI_EXPORT(UV_Timer_timerSetRepeat);
PURS_FFI_EXPORT(UV_Timer_timerGetRepeat);
PURS_FFI_EXPORT(UV_Timer_timerAgainImpl);

#endif // PUREC_UV_TIMER_H
