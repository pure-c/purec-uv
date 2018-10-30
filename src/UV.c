#include <uv.h>
#include <purescript.h>

#define TO_EITHER(EVAL_RET, V)\
	({\
		int __ret = EVAL_RET;\
		(__ret == 0)\
			? purs_any_app(Right, V)\
			: purs_any_app(Left, purs_any_int_new(__ret));\
	})

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

static void alloc_cb(uv_handle_t* handle,
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

void on_connection_cb (uv_stream_t* server, int status) {
    // TODO
}

PURS_FFI_FUNC_6(UV_listenImpl, Left, Right, _stream, _backlog, _cb, _, {
	uv_stream_t * stream = purs_any_get_foreign(_stream)->data;
	int backlog = purs_any_get_int(_backlog);
	return TO_EITHER(uv_listen(stream, backlog, on_connection_cb), NULL);
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
 * Networking: UDP
 ******************************************************************************/

PURS_FFI_VALUE(UV__UdpIpv6Only, PURS_ANY_INT(UV_UDP_IPV6ONLY));
PURS_FFI_VALUE(UV__UdpPartial, PURS_ANY_INT(UV_UDP_PARTIAL));
PURS_FFI_VALUE(UV__UdpReuseAddr, PURS_ANY_INT(UV_UDP_REUSEADDR));

typedef struct purec_udp_ctx_s purec_udp_ctx_t;
struct purec_udp_ctx_s {
	const purs_any_t * on_recv_cont;
	const purs_any_t * Left;
	const purs_any_t * Right;
};

PURS_FFI_FUNC_4(UV_udpNewImpl, Left, Right, _loop, _, {
	uv_loop_t *loop = purs_any_get_foreign(_loop)->data;
	uv_udp_t  *handle = purs_new(uv_udp_t);
	handle->data = purs_new(purec_udp_ctx_t);
	return TO_EITHER(
		uv_udp_init(loop, handle),
		purs_any_foreign_new(NULL, handle));
});

PURS_FFI_FUNC_6(UV_udpBindImpl, Left, Right, _addr, _flags, _handle, _, {
	uv_udp_t *handle = purs_any_get_foreign(_handle)->data;
	int flags = build_flags(_flags);
	const struct sockaddr *addr = purs_any_get_foreign(_addr)->data;
	return TO_EITHER(uv_udp_bind(handle, addr, flags), NULL);
});


static void udp_recv_cb(uv_udp_t* handle,
			ssize_t nread,
			const uv_buf_t* buf,
			const struct sockaddr* addr,
			unsigned flags) {
	const purec_udp_ctx_t * ctx = handle->data;
	assert(ctx->on_recv_cont != NULL);

	/* TODO: check 'flags' for 'UV_UDP_PARTIAL' */

	const purs_any_t * result;

	if (nread < 0) {
		/* transmission error */
		arg = purs_any_app(ctx->Left, nread);
	} else if (nread == 0) {
		if (addr == NULL) {
			/* nothing to read */
		} else {
			/* empty datagram */
		}
	} else {
		/* ordinary datagram */
	}

	purs_any_app(purs_any_app(ctx->on_recv_cont, result), NULL);
}

PURS_FFI_FUNC_5(UV_udpRecvStartImpl, Left, Right, _recvCont, _handle, _, {
	uv_udp_t * handle = purs_any_get_foreign(_handle)->data;
	purec_udp_ctx_t * ctx = handle->data;
	assert(ctx->on_recv_cont == NULL);
	ctx->on_recv_cont = _recvCont;
	ctx->Left = Left;
	ctx->Right = Right;
	return TO_EITHER(uv_udp_recv_start(handle, alloc_cb, udp_recv_cb), NULL);
});

PURS_FFI_FUNC_5(UV_udpSetBroadcastImpl, Left, Right, _on, _handle, _, {
	purs_any_int_t on = purs_any_get_int(_on);
	uv_udp_t * handle = purs_any_get_foreign(_handle)->data;
	return TO_EITHER(uv_udp_set_broadcast(handle, on), NULL);
});

typedef struct purec_udp_send_ctx_s purec_udp_send_ctx_t;
struct purec_udp_send_ctx_s {
	const purs_any_t * Left;
	const purs_any_t * Right;
	const purs_any_t * on_send_cont;
};

void purec_udp_send_cb (uv_udp_send_t* req, int status) {
	const purec_udp_send_ctx_t * ctx = req->data;
	const purs_any_t * Left = ctx->Left;
	const purs_any_t * Right = ctx->Right;
	purs_any_app(purs_any_app(ctx->on_send_cont,
				  TO_EITHER(status, NULL)), NULL);
}

PURS_FFI_FUNC_7(UV_udpSendImpl, Left, Right, _bufs, _addr, _cb, _handle, _, {
	const purs_vec_t * bufs_vec = purs_any_get_array(_bufs);
	const struct sockaddr * addr = purs_any_get_foreign(_addr)->data;
	uv_udp_t * handle = purs_any_get_foreign(_handle)->data;
	purec_udp_send_ctx_t * ctx = purs_new(purec_udp_send_ctx_t);
	ctx->Left = Left;
	ctx->Right = Right;
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
			    purec_udp_send_cb),
		NULL);
});
