#include <uv.h>
#include <purescript.h>

#define RETURN_EITHER(RET, V)\
	return RET == UV_OK\
		? purs_any_app(Right, V)\
		: purs_any_app(Left, purs_any_int_new(RET))\


PURS_FFI_VALUE(UV__RunDefault, PURS_ANY_INT(UV_RUN_DEFAULT));
PURS_FFI_VALUE(UV__RunOnce, PURS_ANY_INT(UV_RUN_ONCE));
PURS_FFI_VALUE(UV__RunNoWait, PURS_ANY_INT(UV_RUN_NOWAIT));

PURS_ANY_THUNK_DEF(
	UV_defaultLoop,
	purs_any_foreign_new(NULL, (void*) uv_default_loop()));

PURS_FFI_FUNC_1(UV_defaultLoop, _, {
	return UV_defaultLoop;
});

PURS_FFI_FUNC_1(UV_newLoop, _, {
	return purs_any_foreign_new(NULL, (void*) uv_loop_new());
});

PURS_FFI_FUNC_5(UV_runImpl, Left, Right, _loop, _mode, _, {
	uv_loop_t* loop = purs_any_get_foreign(_loop)->data;
	uv_run_mode mode = purs_any_get_int(_mode);
	int ret = uv_run(loop, mode);
	RETURN_EITHER(ret, NULL);
});

PURS_FFI_FUNC_4(UV_udpInitImpl, Left, Right, _loop, _, {
	uv_loop_t* loop = purs_any_get_foreign(_loop)->data;
	uv_udp_t * h = purs_new(uv_udp_t);
	RETURN_EITHER(uv_udp_init(loop, h),
		      purs_any_foreign_new(NULL, h));
});

PURS_FFI_FUNC_5(UV_udpBindImpl, Left, Right, _handle, _addr, _, {
	uv_udp_t * handle = purs_any_get_foreign(_handle)->data;
	struct sockaddr_in *addr = purs_any_get_foreign(_addr)->data;
	RETURN_EITHER(uv_udp_bind(handle, *addr, 0),
		      NULL);
});


PURS_FFI_FUNC_2(UV_ip4Addr, _ip, _port, {
	const void *ip = purs_any_get_string(_ip);
	int port = purs_any_get_int(_port);
	struct sockaddr_in *addr = purs_new(struct sockaddr_in);
	struct sockaddr_in tmp = uv_ip4_addr((const char*) ip, port);
	memcpy(addr, &tmp, sizeof (struct sockaddr_in));
	return purs_any_foreign_new(NULL, addr);
});
