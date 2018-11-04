#include <uv.h>
#include <purescript.h>

#include "UV.Internal.h"
#include "UV.Tcp.h"

PURS_FFI_VALUE(UV_Tcp__TcpIpv6Only, PURS_ANY_INT(UV_TCP_IPV6ONLY));

#define UNPACK_TCP_CTX(HANDLE)\
	UNPACK_BASE_CTX((purec_uv_tcp_ctx_t*) (HANDLE->data))

PURS_FFI_FUNC_6(UV_Tcp_tcpNewImpl, Left, Right, Nothing, Just, _loop, _, {
	uv_loop_t *loop = purs_any_get_foreign(_loop)->data;
	uv_tcp_t  *handle = purs_new(uv_tcp_t);
	INIT_BASE_HANDLE(handle, purec_uv_tcp_ctx_t);
	return TO_EITHER(
		uv_tcp_init(loop, handle),
		purs_any_foreign_new(NULL, handle));
});

PURS_FFI_FUNC_4(UV_Tcp_tcpBindImpl, _addr, _flags, _handle, _, {
	uv_tcp_t *handle = purs_any_get_foreign(_handle)->data;
	const struct sockaddr *addr = purs_any_get_foreign(_addr)->data;
	int flags = purec_uv_build_flags(_flags);
	UNPACK_TCP_CTX(handle);
	return TO_EITHER(uv_tcp_bind(handle, addr, flags), NULL);
});

static void purec_uv_tcp_on_connect_cb(uv_connect_t* req, int status) {
	UNPACK_TCP_CTX(req->handle);
	const purs_any_t * cont = req->data;
	assert(cont != NULL);
	purs_any_app(purs_any_app(cont,
				  TO_EITHER(status, NULL)), NULL);
}

PURS_FFI_FUNC_4(UV_Tcp_tcpConnectImpl, _addr, _cb, _handle, _, {
	uv_tcp_t *handle = purs_any_get_foreign(_handle)->data;
	const struct sockaddr *addr = purs_any_get_foreign(_addr)->data;
	UNPACK_TCP_CTX(handle);
	uv_connect_t *req = purs_new(uv_connect_t);
	req->data = (void*) _cb;
	return TO_EITHER(uv_tcp_connect(req,
					handle,
					addr,
					purec_uv_tcp_on_connect_cb),
			 NULL);
});
