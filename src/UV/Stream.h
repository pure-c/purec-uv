#ifndef PUREC_UV_STREAM_H
#define PUREC_UV_STREAM_H

#include <uv.h>
#include <purescript.h>

#include "UV.Internal.h"

#define STREAM_HANDLE_FIELDS\
	BASE_HANDLE_FIELDS\
	const purs_any_t * on_connection_cont;\
	const purs_any_t * on_read_cont;

typedef struct purec_uv_stream_ctx_s purec_uv_stream_ctx_t;
struct purec_uv_stream_ctx_s {
	STREAM_HANDLE_FIELDS
};

PURS_FFI_EXPORT(UV_Stream_listenImpl);
PURS_FFI_EXPORT(UV_Stream_readStartImpl);
PURS_FFI_EXPORT(UV_Stream_writeImpl);

#endif // PUREC_UV_STREAM_H
