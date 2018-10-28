#ifndef PUREC_UV_H
#define PUREC_UV_H

#include <purescript.h>

PURS_FFI_EXPORT(UV__RunDefault);
PURS_FFI_EXPORT(UV__RunOnce);
PURS_FFI_EXPORT(UV__RunNoWait);
PURS_FFI_EXPORT(UV_defaultLoop);
PURS_FFI_EXPORT(UV_newLoop);
PURS_FFI_EXPORT(UV_runImpl);

#endif // PUREC_UV_H
