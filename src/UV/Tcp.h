#ifndef PUREC_UV_TCP_H
#define PUREC_UV_TCP_H

#include <uv.h>
#include <purescript.h>

#include "UV.Internal.h"
#include "UV.Stream.h"

typedef struct purec_uv_tcp_ctx_s purec_uv_tcp_ctx_t;
struct purec_uv_tcp_ctx_s {
	STREAM_HANDLE_FIELDS
};

PURS_FFI_EXPORT(UV_Tcp_tcpNewImpl);
PURS_FFI_EXPORT(UV_Tcp_tcpBindImpl);
PURS_FFI_EXPORT(UV_Tcp_tcpConnectImpl);

#endif // PUREC_UV_TCP_H
