#include <uv.h>
#include <purescript.h>

#define TO_EITHER(EVAL_RET, V)\
	({\
		int __ret = EVAL_RET;\
		(__ret == 0)\
			? purs_any_app(Right, V)\
			: purs_any_app(Left, purs_any_int_new(__ret));\
	})

#define BASE_HANDLE_FIELDS\
	uv_handle_type type;\
	const purs_any_t * Left;\
	const purs_any_t * Right;\
	const purs_any_t * Nothing;\
	const purs_any_t * Just;

typedef struct purec_base_ctx_s purec_base_ctx_t;
struct purec_base_ctx_s {
	BASE_HANDLE_FIELDS
};

#define UNPACK_BASE_CTX(DATA)\
	const purs_any_t * Left = ((purec_base_ctx_t*) (DATA))->Left;\
	const purs_any_t * Right = ((purec_base_ctx_t*) (DATA))->Right;\
	const purs_any_t * Nothing = ((purec_base_ctx_t*) (DATA))->Nothing;\
	const purs_any_t * Just = ((purec_base_ctx_t*) (DATA))->Just;

#define INIT_BASE_HANDLE(HANDLE, STRUCT_TYPE, HANDLE_TYPE)\
	(HANDLE)->data = purs_new(STRUCT_TYPE);\
	((purec_base_ctx_t*)((HANDLE)->data))->type = HANDLE_TYPE;\
	((purec_base_ctx_t*)((HANDLE)->data))->Left = Left;\
	((purec_base_ctx_t*)((HANDLE)->data))->Right = Right;\
	((purec_base_ctx_t*)((HANDLE)->data))->Nothing = Nothing;\
	((purec_base_ctx_t*)((HANDLE)->data))->Just = Just;

PURS_FFI_VALUE(UV__RunDefault, PURS_ANY_INT(UV_RUN_DEFAULT));
PURS_FFI_VALUE(UV__RunOnce, PURS_ANY_INT(UV_RUN_ONCE));
PURS_FFI_VALUE(UV__RunNoWait, PURS_ANY_INT(UV_RUN_NOWAIT));

PURS_ANY_THUNK_DEF(
	UV_defaultLoop,
	purs_any_foreign_new(NULL, (void*) uv_default_loop()));

PURS_FFI_FUNC_1(UV_strerror, _err, {
	return purs_any_string_new(uv_strerror(purs_any_get_int(_err)));
});

PURS_FFI_FUNC_1(UV_defaultLoop, _, {
	return UV_defaultLoop;
});

PURS_FFI_FUNC_1(UV_newLoop, _, {
	return purs_any_foreign_new(NULL, (void*) uv_loop_new());
});

PURS_FFI_FUNC_5(UV_runImpl, Left, Right, _loop, _mode, _, {
	uv_loop_t* loop = purs_any_get_foreign(_loop)->data;
	uv_run_mode mode = purs_any_get_int(_mode);
	return TO_EITHER(uv_run(loop, mode), NULL);
});

/*******************************************************************************
 * Common
 ******************************************************************************/

static void purec__alloc_buf_cb(uv_handle_t* handle,
				size_t suggested_size,
				uv_buf_t* buf) {
	buf->base = purs_malloc(suggested_size);
	buf->len = suggested_size;
}

/*******************************************************************************
 * Utilities
 ******************************************************************************/

int build_flags (const purs_any_t * flags) {
	const purs_vec_t * arr = purs_any_get_array(flags);
	const purs_any_t * tmp;
	int i;
	int output = 0;
	purs_vec_foreach(arr, tmp, i) {
		output |= purs_any_get_int(tmp);
	}
	return output;
}

/*******************************************************************************
 * Streams
 ******************************************************************************/

#define STREAM_HANDLE_FIELDS\
	BASE_HANDLE_FIELDS\
	const purs_any_t * on_connection_cont;

typedef struct purec_stream_ctx_s purec_stream_ctx_t;
struct purec_stream_ctx_s {
	STREAM_HANDLE_FIELDS
};

static void purec__on_read_cb (uv_stream_t* stream,
			       ssize_t nread,
			       const uv_buf_t* buf) {
}

static void purec__on_connection_cb (uv_stream_t *server_handle, int status) {
	purec_stream_ctx_t *ctx = server_handle->data;
	UNPACK_BASE_CTX(server_handle->data);
	assert(ctx->on_connection_cont != NULL);
	switch (ctx->type) {
	case UV_TCP: {
		uv_tcp_t  *client_handle = purs_new(uv_tcp_t);
		uv_tcp_init(server_handle->loop, client_handle);
		purs_any_app(
			purs_any_app(
				ctx->on_connection_cont,
				TO_EITHER(
					uv_accept(server_handle, (uv_stream_t*) client_handle),
					purs_any_foreign_new(NULL, client_handle))),
			NULL);
	}
	default:
		purs_assert(0,
			    "on_connection_cb: NOT IMPLEMENTED (type:%i)",
			    ctx->type);
	}
}

PURS_FFI_FUNC_4(UV_listenImpl, _backlog, _cb, _handle, _, {
	uv_stream_t  *handle = purs_any_get_foreign(_handle)->data;
	int backlog = purs_any_get_int(_backlog);
	UNPACK_BASE_CTX(handle->data);
	purec_stream_ctx_t *ctx = handle->data;
	assert(ctx->on_connection_cont == NULL);
	ctx->on_connection_cont = _cb;
	return TO_EITHER(uv_listen(handle, backlog, purec__on_connection_cb), NULL);
});

/*******************************************************************************
 * Buffers
 ******************************************************************************/

PURS_FFI_FUNC_2(UV_bufferFromString, _str, _, {
	const void *str = purs_any_get_string(_str);
	uv_buf_t *buf = purs_new(uv_buf_t);
	size_t str_size = purs_string_size(str);
	void *base = purs_malloc(str_size);
	memcpy(base, str, str_size);
	buf->len = purs_string_size(str);
	buf->base = base;
	return purs_any_foreign_new(NULL, buf);
});

PURS_FFI_FUNC_2(UV_bufferToString, _buf, _, {
	const uv_buf_t *buf = purs_any_get_foreign(_buf)->data;
	char *dat = purs_malloc(buf->len + 1);
	memcpy(dat, buf->base, buf->len);
	dat[buf->len] = '\0';
	return purs_any_string_new_mv(dat);
});

/*******************************************************************************
 * Networking
 ******************************************************************************/

PURS_FFI_FUNC_2(UV_ip4Addr, _ip, _port, {
	const void *ip = purs_any_get_string(_ip);
	int port = purs_any_get_int(_port);
	struct sockaddr_in *addr = purs_new(struct sockaddr_in);
	uv_ip4_addr((const char*) ip, port, addr);
	return purs_any_foreign_new(NULL, addr);
});

/*******************************************************************************
 * Networking: TCP
 ******************************************************************************/

PURS_FFI_VALUE(UV__TcpIpv6Only, PURS_ANY_INT(UV_TCP_IPV6ONLY));

typedef struct purec_tcp_ctx_s purec_tcp_ctx_t;
struct purec_tcp_ctx_s {
	STREAM_HANDLE_FIELDS
};

#define UNPACK_TCP_CTX(DATA)\
	UNPACK_BASE_CTX((purec_stream_ctx_t*) (DATA))

PURS_FFI_FUNC_6(UV_tcpNewImpl, Left, Right, Nothing, Just, _loop, _, {
	uv_loop_t *loop = purs_any_get_foreign(_loop)->data;
	uv_tcp_t  *handle = purs_new(uv_tcp_t);
	INIT_BASE_HANDLE(handle, purec_tcp_ctx_t, UV_TCP);
	return TO_EITHER(
		uv_tcp_init(loop, handle),
		purs_any_foreign_new(NULL, handle));
});

PURS_FFI_FUNC_4(UV_tcpBindImpl, _addr, _flags, _handle, _, {
	uv_tcp_t *handle = purs_any_get_foreign(_handle)->data;
	const struct sockaddr *addr = purs_any_get_foreign(_addr)->data;
	int flags = build_flags(_flags);
	UNPACK_TCP_CTX(handle->data);
	return TO_EITHER(uv_tcp_bind(handle, addr, flags), NULL);
});

/*******************************************************************************
 * Networking: UDP
 ******************************************************************************/

PURS_FFI_VALUE(UV__UdpIpv6Only, PURS_ANY_INT(UV_UDP_IPV6ONLY));
PURS_FFI_VALUE(UV__UdpPartial, PURS_ANY_INT(UV_UDP_PARTIAL));
PURS_FFI_VALUE(UV__UdpReuseAddr, PURS_ANY_INT(UV_UDP_REUSEADDR));

typedef struct purec_udp_ctx_s purec_udp_ctx_t;
struct purec_udp_ctx_s {
	BASE_HANDLE_FIELDS;
	const purs_any_t * on_recv_cont;
};

#define UNPACK_UDP_CTX(HANDLE)\
	const purs_any_t * Left = ((purec_udp_ctx_t*) HANDLE->data)->Left;\
	const purs_any_t * Right = ((purec_udp_ctx_t*) HANDLE->data)->Right;\
	const purs_any_t * Nothing = ((purec_udp_ctx_t*) HANDLE->data)->Nothing;\
	const purs_any_t * Just = ((purec_udp_ctx_t*) HANDLE->data)->Just;

PURS_FFI_FUNC_6(UV_udpNewImpl, Left, Right, Nothing, Just, _loop, _, {
	uv_loop_t *loop = purs_any_get_foreign(_loop)->data;
	uv_udp_t  *handle = purs_new(uv_udp_t);
	INIT_BASE_HANDLE(handle, purec_udp_ctx_t, UV_UDP);
	return TO_EITHER(
		uv_udp_init(loop, handle),
		purs_any_foreign_new(NULL, handle));
});

PURS_FFI_FUNC_4(UV_udpBindImpl, _addr, _flags, _handle, _, {
	uv_udp_t *handle = purs_any_get_foreign(_handle)->data;
	int flags = build_flags(_flags);
	UNPACK_UDP_CTX(handle);
	const struct sockaddr *addr = purs_any_get_foreign(_addr)->data;
	return TO_EITHER(uv_udp_bind(handle, addr, flags), NULL);
});

static void purec__udp_recv_cb(uv_udp_t* handle,
			       ssize_t nread,
			       const uv_buf_t* buf,
			       const struct sockaddr* addr,
			       unsigned flags) {

	const purec_udp_ctx_t * ctx = handle->data;
	UNPACK_UDP_CTX(handle);

	assert(ctx->on_recv_cont != NULL);

	/* TODO: check 'flags' for 'UV_UDP_PARTIAL' */

	const purs_any_t * result;
	if (nread < 0) {
		/* transmission error */
		result = purs_any_app(Left, purs_any_int_new(nread));
	} else {
		if (nread == 0 && addr == NULL) {
			if (addr == NULL) {
				/* nothing to read */
				result = Nothing;
			} else {
				uv_buf_t *buf_out = purs_new(uv_buf_t);
				buf_out->len = 0;
				buf_out->base = NULL;

				/* empty datagram */
				result = purs_any_app(
					Just,
					purs_any_foreign_new(NULL, buf_out));
			}
		} else {
			/* ordinary datagram */
			uv_buf_t *buf_out = purs_new(uv_buf_t);
			purs_realloc(buf->base, nread);
			buf_out->len = nread;
			buf_out->base = buf->base;
			result = purs_any_app(
				Just,
				purs_any_foreign_new(NULL, buf_out));
		}
	}

	purs_any_app(purs_any_app(ctx->on_recv_cont, result), NULL);
}

PURS_FFI_FUNC_3(UV_udpRecvStartImpl, _recvCont, _handle, _, {
	uv_udp_t * handle = purs_any_get_foreign(_handle)->data;
	purec_udp_ctx_t * ctx = handle->data;
	UNPACK_UDP_CTX(handle)
	assert(ctx->on_recv_cont == NULL);
	ctx->on_recv_cont = _recvCont;
	return TO_EITHER(uv_udp_recv_start(handle, purec__alloc_buf_cb, purec__udp_recv_cb), NULL);
});

PURS_FFI_FUNC_5(UV_udpSetBroadcastImpl, Left, Right, _on, _handle, _, {
	purs_any_int_t on = purs_any_get_int(_on);
	uv_udp_t * handle = purs_any_get_foreign(_handle)->data;
	return TO_EITHER(uv_udp_set_broadcast(handle, on), NULL);
});

typedef struct purec_udp_send_ctx_s purec_udp_send_ctx_t;
struct purec_udp_send_ctx_s {
	const purs_any_t * on_send_cont;
};

void purec__udp_send_cb (uv_udp_send_t* req, int status) {
	const purec_udp_send_ctx_t * ctx = req->data;

	UNPACK_UDP_CTX(req->handle)

	purs_any_app(purs_any_app(ctx->on_send_cont,
				  TO_EITHER(status, NULL)), NULL);
}

PURS_FFI_FUNC_5(UV_udpSendImpl, _bufs, _addr, _cb, _handle, _, {
	const purs_vec_t * bufs_vec = purs_any_get_array(_bufs);
	const struct sockaddr * addr = purs_any_get_foreign(_addr)->data;
	uv_udp_t * handle = purs_any_get_foreign(_handle)->data;

	UNPACK_UDP_CTX(handle)

	purec_udp_send_ctx_t * ctx = purs_new(purec_udp_send_ctx_t);
	ctx->on_send_cont = _cb;

	uv_udp_send_t *req = purs_new(uv_udp_send_t);
	req->data = ctx;

	uv_buf_t* bufs = purs_malloc(bufs_vec->length * sizeof(uv_buf_t));
	{
		int i;
		const purs_any_t *tmp;
		purs_vec_foreach(bufs_vec, tmp, i) {
			/* shallow copy buffer structure */
			const uv_buf_t *buf = purs_any_get_foreign(tmp)->data;
			memcpy(&bufs[i], buf, sizeof(uv_buf_t));
		}
	}
	return TO_EITHER(
		uv_udp_send(req,
			    handle,
			    bufs,
			    bufs_vec->length,
			    addr,
			    purec__udp_send_cb),
		NULL);
});
