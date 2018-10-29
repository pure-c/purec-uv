#ifndef PUREC_UV_H
#define PUREC_UV_H

#include <purescript.h>

PURS_FFI_EXPORT(UV__RunDefault);
PURS_FFI_EXPORT(UV__RunOnce);
PURS_FFI_EXPORT(UV__RunNoWait);
PURS_FFI_EXPORT(UV_defaultLoop);
PURS_FFI_EXPORT(UV_newLoop);
PURS_FFI_EXPORT(UV_runImpl);
PURS_FFI_EXPORT(UV_udpInitImpl);
PURS_FFI_EXPORT(UV_udpBindImpl);
PURS_FFI_EXPORT(UV_ip4Addr);

#endif // PUREC_UV_H
