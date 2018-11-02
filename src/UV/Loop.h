#ifndef PUREC_UV_LOOP_H
#define PUREC_UV_LOOP_H

#include <uv.h>
#include <purescript.h>

PURS_FFI_EXPORT(UV_Loop__RunDefault);
PURS_FFI_EXPORT(UV_Loop__RunOnce);
PURS_FFI_EXPORT(UV_Loop__RunNoWait);
PURS_FFI_EXPORT(UV_Loop_defaultLoop);
PURS_FFI_EXPORT(UV_Loop_newLoop);
PURS_FFI_EXPORT(UV_Loop_runImpl);

#endif // PUREC_UV_LOOP_H
