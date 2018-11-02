#include <uv.h>
#include <purescript.h>

#include "UV.Stream.h"
#include "UV.Tcp.h"
#include "UV.Internal.h"

#define UNPACK_STREAM_CTX(HANDLE)\
	UNPACK_BASE_CTX((purec_uv_stream_ctx_t*) (HANDLE->data))

static void purec_uv_on_connection_cb (uv_stream_t *server_handle, int status) {
	UNPACK_STREAM_CTX(server_handle);
	purec_uv_stream_ctx_t *ctx = server_handle->data;
	assert(ctx->on_connection_cont != NULL);
	switch (server_handle->type) {
	case UV_TCP: {
		uv_tcp_t  *client_handle = purs_new(uv_tcp_t);
		uv_tcp_init(server_handle->loop, client_handle);
		INIT_BASE_HANDLE(client_handle, purec_uv_tcp_ctx_t);
		purs_any_app(
			purs_any_app(
				ctx->on_connection_cont,
				TO_EITHER(
					uv_accept(server_handle,
						  (uv_stream_t*) client_handle),
					purs_any_foreign_new(NULL, client_handle))),
			NULL);
		break;
	}
	default:
		purs_assert(0,
			    "on_connection_cb: NOT IMPLEMENTED (type:%i)",
			    server_handle->type);
	}
}

PURS_FFI_FUNC_4(UV_Stream_listenImpl, _backlog, _cb, _handle, _, {
	uv_stream_t  *handle = purs_any_get_foreign(_handle)->data;
	int backlog = purs_any_get_int(_backlog);
	UNPACK_STREAM_CTX(handle);
	purec_uv_stream_ctx_t *ctx = handle->data;
	assert(ctx->on_connection_cont == NULL);
	ctx->on_connection_cont = _cb;
	return TO_EITHER(uv_listen(handle, backlog, purec_uv_on_connection_cb), NULL);
});

static void purec_uv_on_read_cb (uv_stream_t* handle,
			       ssize_t nread,
			       const uv_buf_t* buf) {
	UNPACK_STREAM_CTX(handle);
	purec_uv_stream_ctx_t *ctx = handle->data;
	assert(ctx->on_read_cont != NULL);

	const purs_any_t * result;
	if (nread == UV_EOF) {
		result = purs_any_app(Right, Nothing);
	} else if (nread < 0) {
		result = purs_any_app(Left, purs_any_int_new(nread));
	} else {
		uv_buf_t *buf_out = purs_new(uv_buf_t);
		purs_realloc(buf->base, nread);
		buf_out->len = nread;
		buf_out->base = buf->base;
		result = purs_any_app(Right,
				      purs_any_app(Just,
						   purs_any_foreign_new(NULL,
									buf_out)));
	}

	purs_any_app(purs_any_app(ctx->on_read_cont, result), NULL);
}

PURS_FFI_FUNC_3(UV_Stream_readStartImpl, _cb, _handle, _, {
	uv_stream_t  *handle = purs_any_get_foreign(_handle)->data;
	UNPACK_STREAM_CTX(handle);
	purec_uv_stream_ctx_t *ctx = handle->data;
	assert(ctx->on_connection_cont == NULL);
	ctx->on_read_cont = _cb;
	return TO_EITHER(uv_read_start(handle,
				       purec_uv_alloc_buf_cb,
				       purec_uv_on_read_cb), NULL);
});

void purec_uv_write_cb (uv_write_t* req, int status) {
	const purs_any_t * cont = req->data;
	UNPACK_STREAM_CTX(req->handle)
	purs_any_app(purs_any_app(cont,
				  TO_EITHER(status, NULL)), NULL);
}

PURS_FFI_FUNC_4(UV_Stream_writeImpl, _bufs, _cb, _handle, _, {
	const purs_vec_t * bufs_vec = purs_any_get_array(_bufs);
	uv_stream_t * handle = purs_any_get_foreign(_handle)->data;
	UNPACK_STREAM_CTX(handle)
	uv_write_t *req = purs_new(uv_write_t);
	req->data = (void*) _cb;

	/* shallow copy buffer structure */
	uv_buf_t* bufs = purs_malloc(bufs_vec->length * sizeof(uv_buf_t));
	{
		int i;
		const purs_any_t *tmp;
		purs_vec_foreach(bufs_vec, tmp, i) {
			const uv_buf_t *buf = purs_any_get_foreign(tmp)->data;
			memcpy(&bufs[i], buf, sizeof(uv_buf_t));
		}
	}

	return TO_EITHER(
		uv_write(req,
			 handle,
			 bufs,
			 bufs_vec->length,
			 purec_uv_write_cb),
		NULL);
});
