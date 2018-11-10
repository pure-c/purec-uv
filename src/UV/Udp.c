#include <uv.h>
#include <purescript.h>

#include "UV.Handle.h"
#include "UV.Internal.h"
#include "UV.Udp.h"

PURS_FFI_VALUE(UV_Udp__UdpIpv6Only, PURS_ANY_INT(UV_UDP_IPV6ONLY));
PURS_FFI_VALUE(UV_Udp__UdpPartial, PURS_ANY_INT(UV_UDP_PARTIAL));
PURS_FFI_VALUE(UV_Udp__UdpReuseAddr, PURS_ANY_INT(UV_UDP_REUSEADDR));

PURS_FFI_FUNC_3(UV_Udp_udpNewImpl, _utils, _loop, _, {
	uv_loop_t *loop = purs_any_get_foreign(_loop)->data;
	uv_udp_t  *handle = purs_new(uv_udp_t);
	INIT_BASE_HANDLE(handle,
			 purec_uv_udp_ctx_t,
			 purs_any_get_foreign(_utils)->data);
	return TO_EITHER(
		HANDLE_GET_UTILS(handle),
		uv_udp_init(loop, handle),
		purs_any_foreign_new(NULL, handle));
});

PURS_FFI_FUNC_4(UV_Udp_udpBindImpl, _addr, _flags, _handle, _, {
	uv_udp_t *handle = purs_any_get_foreign(_handle)->data;
	int flags = purec_uv_build_flags(_flags);
	const struct sockaddr *addr = purs_any_get_foreign(_addr)->data;
	return TO_EITHER(HANDLE_GET_UTILS(handle),
			 uv_udp_bind(handle, addr, flags),
			 NULL);
});

static void purec_uv_udp_recv_cb(uv_udp_t* handle,
				 ssize_t nread,
				 const uv_buf_t* buf,
				 const struct sockaddr* addr,
				 unsigned flags) {

	const purec_uv_udp_ctx_t * ctx = handle->data;
	const purec_uv_utils_t * utils = HANDLE_GET_UTILS(handle);

	assert(ctx->on_recv_cont != NULL);

	/* TODO: check 'flags' for 'UV_UDP_PARTIAL' */

	const purs_any_t * result;
	if (nread < 0) {
		/* transmission error */
		result = purs_any_app(utils->Left,
				      purs_any_int_new(nread));
	} else {
		if (nread == 0 && addr == NULL) {
			if (addr == NULL) {
				/* nothing to read */
				result = utils->Nothing;
			} else {
				uv_buf_t *buf_out = purs_new(uv_buf_t);
				buf_out->len = 0;
				buf_out->base = NULL;

				/* empty datagram */
				result = purs_any_app(
					utils->Just,
					purs_any_foreign_new(NULL, buf_out));
			}
		} else {
			/* ordinary datagram */
			uv_buf_t *buf_out = purs_new(uv_buf_t);
			purs_realloc(buf->base, nread);
			buf_out->len = nread;
			buf_out->base = buf->base;
			result = purs_any_app(
				utils->Just,
				purs_any_foreign_new(NULL, buf_out));
		}
	}

	purs_any_app(purs_any_app(ctx->on_recv_cont, result), NULL);
}

PURS_FFI_FUNC_3(UV_Udp_udpRecvStartImpl, _recvCont, _handle, _, {
	uv_udp_t * handle = purs_any_get_foreign(_handle)->data;
	purec_uv_udp_ctx_t * ctx = handle->data;
	assert(ctx->on_recv_cont == NULL);
	ctx->on_recv_cont = _recvCont;
	return TO_EITHER(HANDLE_GET_UTILS(handle),
			 uv_udp_recv_start(handle,
					   purec_uv_alloc_buf_cb,
					   purec_uv_udp_recv_cb),
			 NULL);
});

PURS_FFI_FUNC_3(UV_Udp_udpSetBroadcastImpl, _on, _handle, _, {
	purs_any_int_t on = purs_any_get_int(_on);
	uv_udp_t * handle = purs_any_get_foreign(_handle)->data;
	return TO_EITHER(HANDLE_GET_UTILS(handle),
			 uv_udp_set_broadcast(handle, on),
			 NULL);
});

void purec_uv_udp_send_cb (uv_udp_send_t* req, int status) {
	const purs_any_t * cont = req->data;
	purs_any_app(purs_any_app(cont,
				  TO_EITHER(HANDLE_GET_UTILS(req->handle),
					    status,
					    NULL)), NULL);
}

PURS_FFI_FUNC_5(UV_Udp_udpSendImpl, _bufs, _addr, _cb, _handle, _, {
	const purs_vec_t * bufs_vec = purs_any_get_array(_bufs);
	const struct sockaddr * addr = purs_any_get_foreign(_addr)->data;
	uv_udp_t * handle = purs_any_get_foreign(_handle)->data;

	uv_udp_send_t *req = purs_new(uv_udp_send_t);
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
		HANDLE_GET_UTILS(handle),
		uv_udp_send(req,
			    handle,
			    bufs,
			    bufs_vec->length,
			    addr,
			    purec_uv_udp_send_cb),
		NULL);
});
